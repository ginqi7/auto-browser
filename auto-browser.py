import asyncio
import json
from time import sleep

import sexpdata
import os
import re

import websocket_bridge_python

from DrissionPage import Chromium
from DrissionPage._elements.none_element import NoneElement
from DrissionPage.common import Keys
from readability import Document
from urllib.parse import urlparse
from collections import OrderedDict

def get_host(url):
    parsed_url = urlparse(url)
    return parsed_url.netloc

def get_tab(trace_id, url, match_host):
    global tabs
    if trace_id in tabs:
        return
    all_tabs = browser.get_tabs()
    for tab in all_tabs:
        print(tab)
        if tab.url == url or \
           get_host(tab.url) == get_host(url):
            tabs[trace_id] = tab
            break

    if  trace_id not in tabs:
        tabs[trace_id] = browser.new_tab(url)
    elif not match_host and \
         tabs[trace_id].url != url:
        tabs[trace_id].get(url)

def locate_element(trace_id, locator, in_element, timeout):
    global tabs, elements
    if not timeout:
        timeout = 1
    if in_element:
        elements[trace_id] = elements[trace_id].ele(locator, timeout=timeout)
    else:
        elements[trace_id] = tabs[trace_id].ele(locator, timeout=timeout)

async def get_element(trace_id, property):
    global tabs, elements
    return [getattr(elements[trace_id], property)]

def handle_arg_types(arg):
    if isinstance(arg, str) and arg.startswith("'"):
        arg = sexpdata.Symbol(arg.partition("'")[2])

    return sexpdata.Quoted(arg)

async def eval_in_emacs(method_name, args):
    args = [sexpdata.Symbol(method_name)] + list(map(handle_arg_types, args))    # type: ignore
    sexp = sexpdata.dumps(args)
    # print(sexp)
    await bridge.eval_in_emacs(sexp)

def run_js(trace_id, js):
    global elements
    element = elements[trace_id]
    if not isinstance(element, NoneElement):
        print(element)
        element.run_js(js)

def run_util_js(tab_id, util_name):
    global tabs, utils_directory
    full_path = os.path.join(utils_directory, util_name)
    tabs[tab_id].run_js(full_path)

def rewrite_image_to_base64(tab_id):
    global tabs, utils_directory
    for el in tabs[tab_id].eles('tag:img'):
        for i in range(10):
            if el.run_js("return this.complete") :
                break
            sleep(0.2)
        full_path = os.path.join(utils_directory, "rewrite-image-to-base64.js")
        el.run_js(full_path)


def delete_oldest():
    global tabs, elements
    if len(tabs) > 50:
        tabs.popitem(last=False)
    if len(elements) > 50:
        elements.popitem(last=False)

def readability_html(html):
    doc = Document(html)
    return [doc.summary()]

def input(trace_id, str, clear):
    global tabs, elements
    elements[trace_id].input(str, clear=clear)

def stream_response_finished(trace_id, kwargs):
    global request_status
    if kwargs['requestId'] in request_status:
        request_status[kwargs['requestId']] = True

    print(kwargs)

def stream_response_filter(trace_id, url_pattern, kwargs):
    global request_status

    # print(kwargs)
    if 'request' in kwargs:
        if 'url' in kwargs['request']:
            base_path = os.path.basename(kwargs['request']['url'])
            request_status[kwargs['requestId']] = False
            if re.match(url_pattern, base_path):
                tabs[trace_id].listen._driver.set_callback("Network.loadingFinished",
                                                           lambda **kwargs: stream_response_finished(trace_id, kwargs),
                                                           True)
                tabs[trace_id].listen._driver.run("Network.streamResourceContent", requestId=kwargs['requestId'])

async def stream_response_handle(trace_id, callback, kwargs):

    if 'data' in kwargs:
        # print(kwargs)
        base64_string = kwargs['data']
        args = [trace_id, base64_string]
        await eval_in_emacs(callback, args)

def stream_response_wait():
    global request_status
    for i in range(1500): # 300 seconds
        sleep(0.2)
        if request_status:
            print(request_status)
            if all(request_status.values()):
                return


def stream_response(trace_id, url_pattern, callback):
    global request_status
    request_status = {}
    print(url_pattern)
    tabs[trace_id].listen.start(url_pattern, True)
    tabs[trace_id].listen._driver.set_callback("Network.requestWillBeSent",
                                               lambda **kwargs: stream_response_filter(trace_id, url_pattern, kwargs),
                                               True)
    tabs[trace_id].listen._driver.set_callback("Network.dataReceived",
                                               lambda **kwargs: asyncio.run(stream_response_handle(trace_id, callback, kwargs)),
                                               True)
    stream_response_wait()
    tabs[trace_id].listen.stop()

def wait_response(trace_id, url_pattern):
    print(url_pattern)
    tabs[trace_id].listen.start(url_pattern, True)
    res = tabs[trace_id].listen.wait()
    data = res.response.body
    if type(data) is bytes:
        data = data.decode('utf-8')
    tabs[trace_id].listen.stop()
    print(data)
    return [data]

def click(trace_id):
    global tabs, elements
    print(elements[trace_id])
    elements[trace_id].click()

def scroll(trace_id, delta_y, delta_x):
    global tabs, elements
    tabs[trace_id].actions.scroll(delta_y=delta_y, delta_x=delta_x)

def key_down(trace_id, key):
    global tabs, elements
    tabs[trace_id].actions.key_down(key)

def key_up(trace_id, key):
    global tabs, elements
    tabs[trace_id].actions.key_up(key)

def refresh(trace_id):
    global tabs, elements
    tabs[trace_id].refresh()

def console(trace_id, script):
    global tabs, elements
    tab = tabs[trace_id]
    tab.console.start()
    tab.run_js(script+';console.log("Finished");',timeout=10)
    data = tab.console.wait()
    tab.console.stop()
    text = data.text
    return [text]

# dispatch message received from Emacs.
async def on_message(message):
    try:
        info = json.loads(message)
        print(info)
        cmd = info[1][0].strip()
        trace_id = info[1][1]
        result = None
        delete_oldest()
        if cmd == 'get-tab':
            url = info[1][2]
            match_host = False
            if len(info[1]) >= 4:
                match_host = info[1][3]
            get_tab(trace_id, url, match_host)
        elif cmd == 'locate-element':
            locator = info[1][2]
            in_element = False
            if (len (info[1]) > 3) :
                in_element = info[1][3]
            if (len (info[1]) > 4) :
                timeout = info[1][4]

            locate_element(trace_id, locator, in_element, timeout)
        elif cmd == 'run-js':
            js = info[1][2]
            run_js(trace_id, js)
        elif cmd == 'get-element':
            property = info[1][2]
            result = await get_element(trace_id, property)
        elif cmd == 'run-util-js':
            util_name = info[1][2]
            run_util_js(trace_id, util_name)
        elif cmd == 'rewrite-image-to-base64':
            rewrite_image_to_base64(trace_id)
        elif cmd == 'readability':
            html = info[1][2]
            result = readability_html(html)
        elif cmd == 'input':
            input_str = info[1][2]
            clear = False
            if (len (info[1]) > 3) :
                clear = info[1][3]
            input(trace_id, input_str, clear)
        elif cmd == 'wait-response':
            url_pattern = info[1][2]
            result = wait_response(trace_id, url_pattern)
        elif cmd == 'stream-response':
            url_pattern = info[1][2]
            callback = info[1][3]
            result = stream_response(trace_id, url_pattern, callback)
        elif cmd == 'click':
            result = click(trace_id)
        elif cmd == 'scroll':
            delta_y = info[1][2]
            delta_x = info[1][3]
            result = scroll(trace_id, delta_y, delta_x)
        elif cmd == 'key-down':
            key = info[1][2]
            result = key_down(trace_id, key)
        elif cmd == 'key-up':
            key = info[1][2]
            result = key_up(trace_id, key)
        elif cmd == 'console':
            script = info[1][2]
            result = console(trace_id, script)
        elif cmd == 'refresh':
            result = refresh(trace_id)

        else:
            print(f'not fount handler for {cmd}', flush=True)
        args = [trace_id]
        if result :
            args = args + result
            # print(args)
        await eval_in_emacs("auto-browser--run-linearly", args)
    except Exception as _:
        import traceback
        print(traceback.format_exc())


# eval in emacs and log the command.
async def run_and_log(cmd):
    print(cmd, flush=True)
    await bridge.eval_in_emacs(cmd)


async def main():
    global bridge
    bridge = websocket_bridge_python.bridge_app_regist(on_message)
    await asyncio.gather(init(), bridge.start())


async def get_emacs_var(var_name: str):
    "Get Emacs variable and format it."
    var_value = await bridge.get_emacs_var(var_name)
    if isinstance(var_value, str):
        var_value = var_value.strip('"')
    print(f'{var_name} : {var_value}')
    if var_value == 'null':
        return None
    return var_value


async def init():
    "Init User data."
    global browser, tabs, elements, utils_directory
    utils_directory = await get_emacs_var("auto-browser-utils-directory")
    browser = Chromium(9222)
    tabs = OrderedDict()
    elements = OrderedDict()
    print('Init')

asyncio.run(main())
