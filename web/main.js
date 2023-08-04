// For testing the Outputs
import { initSync, wasm_main } from "./pkg/flora.js";

const fetch_source = async (path) =>
    await fetch(path);

const wasmInstance = (wasmModule) =>
    new WebAssembly.Module(wasmModule);

const get_exports = (instance) => instance.instance.exports;

function read_chars(values, ptr) {
    const chars = [];

    let i = ptr;
    while (values[i] != 0) {
        chars.push(values[i]);
        i += 1;
    }

    return chars;
}

const string_from_chars = (chars) =>
    chars.map(char => String.fromCharCode(char)).join("");

const log_str = (mem, ptr) => console.log(string_from_chars(read_chars(mem, ptr)));

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
    editor: () => document.getElementById("editor"),
};

(async () => {
    const reader = new Reader();
    const imports = {
        console: {
            log_str: (ptr) => log_str(reader.get_mem(), ptr),
            log: (n) => console.log(n),
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
                console.log(exports);

                return ptr;
            }
        }
    };


    const wasm = await fetch_source("./pkg/flora_bg.wasm");
    const buf = await wasm.arrayBuffer();
    const mod = wasmInstance(buf);

    initSync(mod);

    DOM.compile_btn.addEventListener("click", async () => {
        const result = wasm_main(DOM.editor().value);

        const program = wasmInstance(result);

        const wasm = await WebAssembly.instantiate(program, imports);

        wasm.exports.main();
    });
})()
    .catch(x => console.error(x))
