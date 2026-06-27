// -*- js-indent-level: 4 -*-
// Copyright 2024 Jessica Tallon
// Copyright 2024 Juliana Sims <juli@incana.org>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitati måsteons under the License.

let bindings = {
    webSocket: {
        close: (ws) => ws.close(),
        new(url) {
            ws = new WebSocket(url);
            ws.binaryType = "arraybuffer";
            return ws;
        },
        send: (ws, data) => ws.send(data),
        setOnOpen(ws, f) {
            ws.onopen = (e) => {
                f();
            };
        },
        setOnMessage(ws, f) {
            ws.onmessage = (e) => {
                f(e.data);
            };
        },
        setOnClose(ws, f) {
            ws.onclose = (e) => {
                f(e.code, e.reason);
            };
        },
        setOnError(ws, f) {
            ws.onerror = (e) => f();
        }
    },
    localStorage: {
        setItem: (name, data) => localStorage.setItem(name, data),
        getItem: (name) => localStorage.getItem(name),
    },
    uint8Array: {
        new: (length) => new Uint8Array(length),
        fromArrayBuffer: (buffer) => new Uint8Array(buffer),
        length: (array) => array.length,
        ref: (array, index) => array[index],
        set: (array, index, value) => array[index] = value
    },
    crypto: {
        digest: (algorithm, data) => globalThis.crypto.subtle
            .digest(algorithm, data).then((arrBuf) => new Uint8Array(arrBuf)),
        randomValues(length) {
            const array = new Uint8Array(length);
            globalThis.crypto.getRandomValues(array);
            return array;
        },
        generateEd25519KeyPair: () => globalThis.crypto.subtle.generateKey(
            { name: "Ed25519" },
            true,
            ["sign", "verify"]
        ),
        keyPairPrivateKey: (keyPair) => keyPair.privateKey,
        keyPairPublicKey: (keyPair) => keyPair.publicKey,
        exportKey: (key) => globalThis.crypto.subtle.exportKey("raw", key)
            .then((arrBuf) => new Uint8Array(arrBuf)),
        importPublicKey: (key) => globalThis.crypto.subtle.importKey(
            "raw",
            key,
            { name: "Ed25519" },
            true,
            ["verify"]
        ),
        signEd25519: (data, privateKey) => globalThis.crypto.subtle.sign(
            { name: "Ed25519" },
            privateKey,
            data
        ).then((arrBuf) => new Uint8Array(arrBuf)),
        verifyEd25519: (signature, data, publicKey) => globalThis.crypto.subtle
            .verify(
                { name: "Ed25519" },
                publicKey,
                signature,
                data
            )
    },
    indexedDB: {
        dbOpen: (name, upgrade) => new Promise((resolve, reject) => {
            const request = indexedDB.open(name);
            request.addEventListener("success", (e) => resolve(request.result));
            request.addEventListener("error", (e) => reject());
            request.addEventListener("upgradeneeded", (e) => upgrade(request.result));
        }),
        dbCreateObjectStore: (db, name) => db.createObjectStore(name),
        dbTransaction: (db, name, mode) => db.transaction(name, mode),
        dbTransaction2: (db, name1, name2, mode) => db.transaction([name1, name2], mode),
        objectStoreCount: (store) => new Promise((resolve, reject) => {
            const request = store.count();
            request.addEventListener("success", (e) => resolve(request.result));
            request.addEventListener("error", (e) => reject());
        }),
        objectStoreForEach: (store, f) => new Promise((resolve, reject) => {
            store.openCursor().onsuccess = (e) => {
                const cursor = event.target.result;
                if (cursor) {
                    f(cursor.key, cursor.value);
                    cursor.continue();
                } else {
                    resolve();
                }
            };
        }),
        objectStoreGet: (store, k) => new Promise((resolve, reject) => {
            const request = store.get(k);
            request.addEventListener("success", (e) => resolve(request.result));
            request.addEventListener("error", (e) => reject());
        }),
        objectStorePut: (store, k, v) => store.put(v, k),
        transactionObjectStore: (t, name) => t.objectStore(name),
        transactionCommit: (t) => new Promise((resolve, reject) => {
            t.addEventListener("complete", (e) => resolve());
            t.addEventListener("error", (e) => reject());
            t.commit();
        })
    }
};

if (typeof exports !== 'undefined') {
    // NodeJS runtime.
    exports.user_imports = bindings;
}

// Other EcmaScript runtimes.
export const user_imports = bindings;
