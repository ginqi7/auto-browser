import asyncio
import json
from time import sleep
import sexpdata
import os

import websocket_bridge_python

from DrissionPage import Chromium
from readability import Document
from urllib.parse import urlparse
from collections import OrderedDict

tabs = OrderedDict()
elements = OrderedDict()

def get_host(url):
    parsed_url = urlparse(url)
    return parsed_url.netloc


def get_tab(trace_id, url):
    global tabs
    if trace_id in tabs:
        return
    all_tabs = browser.get_tabs()
    for tab in all_tabs:
        print(tab)
        if tab.url == url or \
           get_host(tab.url) == get_host(url) :
            tabs[trace_id] = tab
            break
        
    if  trace_id not in tabs:
        tabs[trace_id] = browser.new_tab(url)
    elif tabs[trace_id].url != url :
        tabs[trace_id].get(url)
        
def locate_element(trace_id, locator):        
    global tabs, elements
    elements[trace_id] = tabs[trace_id].ele(locator)
    
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
    await bridge.eval_in_emacs(sexp)
    
def run_js(trace_id, js):
    global elements
    elements[trace_id].run_js(js)
    
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
    
def clear(trace_id):
    global tabs, elements
    del(tabs[trace_id])
    del(elements[trace_id])
    
def delete_oldest():
    if len(tabs) > 50:
        tabs.popitem(last=False)
    if len(elements) > 50:
        elements.popitem(last=False)
    
def readability_html(html):
    doc = Document(html)
    return [doc.summary()]

def input(trace_id, str):
    global tabs, elements
    elements[trace_id].input(str)
    
    
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
            get_tab(trace_id, url)
        elif cmd == 'locate-element':
            locator = info[1][2]
            locate_element(trace_id, locator)
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
            input(trace_id, input_str)
        elif cmd == 'wait-response':
            url_pattern = info[1][2]
            result = wait_response(trace_id, url_pattern)
        else:
            print(f'not fount handler for {cmd}', flush=True)
        args = [trace_id]
        if result :
            args = args + result
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
    tabs = {}
    elements = {}
    print('Init')


asyncio.run(main())
