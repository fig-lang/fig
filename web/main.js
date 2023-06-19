// For testing the Outputs

const fetch_source = async (path) =>
    await fetch(path);

const wasmInstance = async (wasmModule, imports) =>
    await WebAssembly.instantiateStreaming(wasmModule, imports);

const get_exports = (instance) => instance.instance.exports;

const read_chars = (values, ptr) => {
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

const $log_str = (mem, ptr) => console.log(string_from_chars(read_chars(mem, ptr)));

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

(async () => {
    const reader = new Reader();
    const imports = {
        js: {
            log: (x) => console.log(x),
            log_str: (ptr) => $log_str(reader.get_mem(), ptr),
        },
        env: {
            sum: (x, y) => x + y
        }
    };

    const wasm = await fetch_source("./external.wasm");
    const exports = await wasmInstance(wasm, imports).then(ins => get_exports(ins));
    reader.set_mem(new Uint8Array(exports.memory.buffer));

    exports.main();
})()
    .catch(x => console.error(x))
