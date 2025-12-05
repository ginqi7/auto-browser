# /// script
# dependencies = [
#     "playwright",
#     "lxml-html-clean>=0.4.1",
#     "readability-lxml>=0.8.1",
#     "sexpdata>=1.0.2",
#     "websocket-bridge-python>=0.0.2",
# ]
# ///

import asyncio
import json
from time import sleep
from typing import final

import sexpdata
import os
import re

import websocket_bridge_python

from playwright.async_api import async_playwright
from readability import Document
from urllib.parse import urlparse
from collections import OrderedDict


def get_host(url) -> str:
    """Get host from URL."""
    return urlparse(url).netloc


async def get_pages():
    default_context = await context()
    return default_context.pages


async def get_page(trace_id, url, match_host):
    global pages
    # if trace_id in pages:
    #     return
    all_pages = await get_pages()
    for page in all_pages:
        print(page)
        if page.url == url or get_host(page.url) == get_host(url):
            pages[trace_id] = page
            break
    if trace_id not in pages:
        pages[trace_id] = await (await context()).new_page()
        await pages[trace_id].goto(url)
    elif not match_host and pages[trace_id].url != url:
        await pages[trace_id].goto(url)


async def locate_element(trace_id, locator_str, nth, timeout):
    global pages, elements
    if timeout is None:
        timeout = 1
    if nth is None:
        nth = 0
    locator = pages[trace_id].locator(locator_str)
    if await locator.count() > 0:
        elements[trace_id] = locator.nth(nth)
    else:
        elements[trace_id] = None


async def get_element(trace_id, property):
    global pages, elements
    element = elements[trace_id]
    if element:
        if property == "html":
            return [await element.evaluate("el => el.outerHTML")]
        elif property == "href":
            return [await element.evaluate("el => el.href")]
    else:
        return ["The element does not exist."]
    return ""


def handle_arg_types(arg):
    if isinstance(arg, str) and arg.startswith("'"):
        arg = sexpdata.Symbol(arg.partition("'")[2])

    return sexpdata.Quoted(arg)


async def eval_in_emacs(method_name, args):
    args = [sexpdata.Symbol(method_name)] + list(map(handle_arg_types, args))  # type: ignore
    sexp = sexpdata.dumps(args)
    # print(sexp)
    await bridge.eval_in_emacs(sexp)


async def run_js(trace_id, js):
    global elements
    element = elements[trace_id]
    if element:
        await element.evaluate(js)


def read_file_contents(file_path):
    try:
        with open(file_path, "r", encoding="utf-8") as file:
            contents = file.read()
            return contents
    except FileNotFoundError:
        print(f"File not found.: {file_path}")
    except IOError:
        print(f"Error reading the file.: {file_path}")


async def run_util_js(tab_id, util_name):
    global pages, utils_directory
    full_path = os.path.join(utils_directory, util_name)
    content = read_file_contents(full_path)
    await pages[tab_id].evaluate(f"() => { {content} }")


async def rewrite_image_to_base64(tab_id):
    global pages, utils_directory
    for el in await pages[tab_id].query_selector_all("img"):
        for i in range(10):
            if await el.evaluate("el => el.complete"):
                break
            sleep(0.2)
        full_path = os.path.join(utils_directory, "rewrite-image-to-base64.js")
        content = read_file_contents(full_path)
        await el.evaluate(content)


def delete_oldest():
    global pages, elements
    if len(pages) > 50:
        pages.popitem(last=False)
    if len(elements) > 50:
        elements.popitem(last=False)


def readability_html(html):
    doc = Document(html)
    return [doc.summary()]


def event_stream_parse(text):
    lines = list(filter(lambda line: line.startswith("data: "), text.split("\n")))
    json_str = lines[-2][6:]
    data = json.loads(json_str)["data"]["aiText"]
    return data


async def input(trace_id, str, enter, response_match):
    global pages, elements
    await elements[trace_id].fill(str)
    if enter and response_match:
        async with pages[trace_id].expect_response(response_match) as response_info:
            await elements[trace_id].press("Enter")
        response = await response_info.value
        text = await response.text()
        rightText = text.encode("windows-1252", errors="ignore").decode(
            "utf-8", errors="ignore"
        )
        data = event_stream_parse(rightText)
        return [data]
    if enter:
        await elements[trace_id].press("Enter")
    return ""


async def monitor_element(trace_id, selector, callback):
    global pages, elements
    while True:
        try:
            # 找到定位器
            element_locator = pages[trace_id].locator(selector)

            # 检查元素是否可见
            if await element_locator.is_visible():
                print(
                    f"🔥🔥🔥 [Monitoring Task Alert]! Element '{selector}' has appeared!"
                )
                args = [trace_id]
                await eval_in_emacs(callback, args)
                break

            await asyncio.sleep(0.5)

        except Exception as e:
            # 如果页面关闭或发生其他错误，优雅地退出循环
            print(f"❌ [Monitoring Task Error] Monitoring task exited: {e}")
            break


def stream_response_finished(trace_id, kwargs):
    global request_status
    if kwargs["requestId"] in request_status:
        request_status[kwargs["requestId"]] = True

    print(kwargs)


def stream_response_filter(trace_id, url_pattern, kwargs):
    global request_status

    # print(kwargs)
    if "request" in kwargs:
        if "url" in kwargs["request"]:
            base_path = os.path.basename(kwargs["request"]["url"])
            request_status[kwargs["requestId"]] = False
            if re.match(url_pattern, base_path):
                pages[trace_id].listen._driver.set_callback(
                    "Network.loadingFinished",
                    lambda **kwargs: stream_response_finished(trace_id, kwargs),
                    True,
                )
                pages[trace_id].listen._driver.run(
                    "Network.streamResourceContent", requestId=kwargs["requestId"]
                )


async def stream_response_handle(trace_id, callback, kwargs):
    if "data" in kwargs:
        # print(kwargs)
        base64_string = kwargs["data"]
        args = [trace_id, base64_string]
        await eval_in_emacs(callback, args)


def stream_response_wait():
    global request_status
    for i in range(1500):  # 300 seconds
        sleep(0.2)
        if request_status:
            print(request_status)
            if all(request_status.values()):
                return


def stream_response(trace_id, url_pattern, callback):
    global request_status
    request_status = {}
    print(url_pattern)
    pages[trace_id].listen.start(url_pattern, True)
    pages[trace_id].listen._driver.set_callback(
        "Network.requestWillBeSent",
        lambda **kwargs: stream_response_filter(trace_id, url_pattern, kwargs),
        True,
    )
    pages[trace_id].listen._driver.set_callback(
        "Network.dataReceived",
        lambda **kwargs: asyncio.run(
            stream_response_handle(trace_id, callback, kwargs)
        ),
        True,
    )
    stream_response_wait()
    pages[trace_id].listen.stop()


def wait_response(trace_id, url_pattern):
    print(url_pattern)
    pages[trace_id].listen.start(url_pattern, True)
    res = pages[trace_id].listen.wait()
    data = res.response.body
    if type(data) is bytes:
        data = data.decode("utf-8")
    pages[trace_id].listen.stop()
    print(data)
    return [data]


async def click(trace_id):
    global pages, elements
    print(elements[trace_id])
    await elements[trace_id].click()


def scroll(trace_id, delta_y, delta_x):
    global pages, elements
    pages[trace_id].actions.scroll(delta_y=delta_y, delta_x=delta_x)


async def key_down(trace_id, key):
    global pages, elements
    await pages[trace_id].keyboard.down(key)


async def key_up(trace_id, key):
    global pages, elements
    await pages[trace_id].keyboard.up(key)


def refresh(trace_id):
    global pages, elements
    pages[trace_id].reload()


async def console(trace_id, script):
    global pages, elements
    page = pages[trace_id]
    async with page.expect_console_message() as msg_info:
        # Issue console.log inside the page
        await page.evaluate(script)
    msg = await msg_info.value
    return [msg]


# dispatch message received from Emacs.
async def on_message(message):
    try:
        info = json.loads(message)
        print(info)
        cmd = info[1][0].strip()
        trace_id = info[1][1]
        result = None
        delete_oldest()
        if cmd == "get-page":
            url = info[1][2]
            match_host = False
            if len(info[1]) >= 4:
                match_host = info[1][3]
            await get_page(trace_id, url, match_host)
        elif cmd == "locate-element":
            locator = info[1][2]
            nth = None
            if len(info[1]) > 3:
                nth = info[1][3]
            if len(info[1]) > 4:
                timeout = info[1][4]
            await locate_element(trace_id, locator, nth, timeout)
        elif cmd == "run-js":
            js = info[1][2]
            await run_js(trace_id, js)
        elif cmd == "get-element":
            property = info[1][2]
            result = await get_element(trace_id, property)
        elif cmd == "run-util-js":
            util_name = info[1][2]
            await run_util_js(trace_id, util_name)
        elif cmd == "rewrite-image-to-base64":
            await rewrite_image_to_base64(trace_id)
        elif cmd == "readability":
            html = info[1][2]
            result = readability_html(html)
        elif cmd == "input":
            input_str = info[1][2]
            enter = False
            if len(info[1]) > 3:
                enter = info[1][3]
            if len(info[1]) > 4:
                response_match = info[1][4]
            result = await input(trace_id, input_str, enter, response_match)
        elif cmd == "wait-response":
            url_pattern = info[1][2]
            result = wait_response(trace_id, url_pattern)
        elif cmd == "stream-response":
            url_pattern = info[1][2]
            callback = info[1][3]
            result = stream_response(trace_id, url_pattern, callback)
        elif cmd == "click":
            result = await click(trace_id)
        elif cmd == "scroll":
            delta_y = info[1][2]
            delta_x = info[1][3]
            result = scroll(trace_id, delta_y, delta_x)
        elif cmd == "key-down":
            key = info[1][2]
            result = await key_down(trace_id, key)
        elif cmd == "key-up":
            key = info[1][2]
            result = await key_up(trace_id, key)
        elif cmd == "console":
            script = info[1][2]
            result = await console(trace_id, script)
        elif cmd == "monitor":
            selector = info[1][2]
            callback = info[1][3]
            result = await monitor_element(trace_id, selector, callback)
        elif cmd == "refresh":
            result = refresh(trace_id)

        else:
            print(f"not fount handler for {cmd}", flush=True)
        args = [trace_id]
        if result:
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
    print(f"{var_name} : {var_value}")
    if var_value == "null":
        return None
    return var_value


async def init():
    "Init User data."
    global browser, pages, elements, utils_directory
    utils_directory = await get_emacs_var("auto-browser-utils-directory")
    playwright = await async_playwright().start()
    browser = await playwright.chromium.connect_over_cdp("http://localhost:9222")
    pages = OrderedDict()
    elements = OrderedDict()
    print("Init")


async def context():
    contexts = browser.contexts
    if len(contexts) < 1:
        await reconnect()
        contexts = browser.contexts
    return contexts[0]


async def reconnect():
    global browser
    browser = await playwright.chromium.connect_over_cdp("http://localhost:9222")


asyncio.run(main())
