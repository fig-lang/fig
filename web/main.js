// For testing the Outputs
import { initSync, wasm_main } from "./pkg/flora.js";

const PRELUDE = `external console { fn log(n: i32); fn log_str(s: string); }`;

const MAIN_EXAMPLE = `export fn main() {
    let x: string = "Hello World";
    log_str(x);
}
`;

const fetch_source = async (path) =>
    await fetch(path);

const wasmInstance = (wasmModule) =>
    new WebAssembly.Module(wasmModule);

function read_chars(values, ptr) {
    let chars = [];
    let n = ptr;

    do {
        chars.push(values.getInt8(n));
        n += 1;
    } while (values.getInt8(n) !== 0);

    return chars;
}

function log_str(mem, ptr) {
    push_to_console(string_from_chars(read_chars(mem, ptr)));
}

class Reader {
    mem = null;

    constructor() {
        this.mem = null;
    }

    set_mem(exported_memory) {
        this.mem = exported_memory;
    }

    get_mem() {
        return this.mem;
    }
}

const DOM = {
    compile_btn: document.getElementById("compile-btn"),
    editor: document.getElementById("editor"),

    info: {
        memory_view: document.querySelector(".memory"),
    },

    console: document.getElementById("console")
};

/**
 * @param {string} value 
 */
function push_to_console(value) {
    const item_element = document.createElement("p");
    item_element.innerHTML = value;

    DOM.console.append(item_element);
}

function push_to_memory_view(item, str = false, bgcolor = "#a0a0a0") {
    const item_element = document.createElement("div");
    item_element.className = "item";
    item_element.style.background = bgcolor;
    item_element.innerHTML = `${str ? String.fromCharCode(item) : item}`;

    DOM.info.memory_view.append(item_element);
}

function clear_memory_view() {
    DOM.info.memory_view.innerHTML = "";
}

let prev_mem;

/**
 * @param {DataView} array 
 */
function update_memory_view(array) {
    clear_memory_view();

    let n = 0;
    do {
        if (prev_mem && prev_mem.getInt8(n) !== array.getInt8(n)) {
            push_to_memory_view(array.getInt8(n), false, `rgb(231, 188, 107)`);
        }
        else {
            push_to_memory_view(array.getInt8(n), false);
        }
        n += 1;
    } while (array.getInt16(n) !== 0);

    prev_mem = array;
}

function string_from_chars(chars) {
    return chars.map(char => String.fromCharCode(char)).join("");
}

(async () => {
    const reader = new Reader();
    const imports = {
        console: {
            log_str: (ptr) => log_str(reader.get_mem(), ptr),
            log: (n) => push_to_console(n),
        },
        element: {
            // element ptr and text ptr
            create: (el, text) => {
                const element = string_from_chars(read_chars(reader.get_mem(), el));
                const child = document.createElement(element);
                const element_body = string_from_chars(read_chars(reader.get_mem(), text));
                child.innerText = element_body;
                window.document.body.appendChild(child);
            }
        },
        io: {
            fetch: async (url_ptr) => {
                const url = string_from_chars(read_chars(reader.get_mem(), url_ptr));
                const res = await fetch(url);
                const content = await res.text();

                const encoder = new TextEncoder();
                const encodedString = encoder.encode(content);

                const ptr = exports.malloc_wrapper(encodedString.length + 1);

                return ptr;
            }
        }
    };

    DOM.editor.value = MAIN_EXAMPLE;

    const wasm = await fetch_source("./pkg/flora_bg.wasm");
    const buf = await wasm.arrayBuffer();
    const mod = wasmInstance(buf);

    initSync(mod);

    DOM.compile_btn.addEventListener("click", async () => {
        DOM.console.innerHTML = "";
        try {
            const result = wasm_main(PRELUDE + DOM.editor.value);
            const program = wasmInstance(result);

            const wasm = await WebAssembly.instantiate(program, imports);
            reader.set_mem(new DataView(wasm.exports.memory.buffer));

            wasm.exports.main();

            update_memory_view(new DataView(wasm.exports.memory.buffer))
        } catch (error) {
            console.log(error);
        }
    });
})()
    .catch(x => console.error(x))
