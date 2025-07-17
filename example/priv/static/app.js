// build/dev/javascript/prelude.mjs
class CustomType {
  withFields(fields) {
    let properties = Object.keys(this).map((label) => (label in fields) ? fields[label] : this[label]);
    return new this.constructor(...properties);
  }
}

class List {
  static fromArray(array, tail) {
    let t = tail || new Empty;
    for (let i = array.length - 1;i >= 0; --i) {
      t = new NonEmpty(array[i], t);
    }
    return t;
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  atLeastLength(desired) {
    let current = this;
    while (desired-- > 0 && current)
      current = current.tail;
    return current !== undefined;
  }
  hasLength(desired) {
    let current = this;
    while (desired-- > 0 && current)
      current = current.tail;
    return desired === -1 && current instanceof Empty;
  }
  countLength() {
    let current = this;
    let length = 0;
    while (current) {
      current = current.tail;
      length++;
    }
    return length - 1;
  }
}
function prepend(element, tail) {
  return new NonEmpty(element, tail);
}
function toList(elements, tail) {
  return List.fromArray(elements, tail);
}

class ListIterator {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
}

class Empty extends List {
}

class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
}

class BitArray {
  bitSize;
  byteSize;
  bitOffset;
  rawBuffer;
  constructor(buffer, bitSize, bitOffset) {
    if (!(buffer instanceof Uint8Array)) {
      throw globalThis.Error("BitArray can only be constructed from a Uint8Array");
    }
    this.bitSize = bitSize ?? buffer.length * 8;
    this.byteSize = Math.trunc((this.bitSize + 7) / 8);
    this.bitOffset = bitOffset ?? 0;
    if (this.bitSize < 0) {
      throw globalThis.Error(`BitArray bit size is invalid: ${this.bitSize}`);
    }
    if (this.bitOffset < 0 || this.bitOffset > 7) {
      throw globalThis.Error(`BitArray bit offset is invalid: ${this.bitOffset}`);
    }
    if (buffer.length !== Math.trunc((this.bitOffset + this.bitSize + 7) / 8)) {
      throw globalThis.Error("BitArray buffer length is invalid");
    }
    this.rawBuffer = buffer;
  }
  byteAt(index) {
    if (index < 0 || index >= this.byteSize) {
      return;
    }
    return bitArrayByteAt(this.rawBuffer, this.bitOffset, index);
  }
  equals(other) {
    if (this.bitSize !== other.bitSize) {
      return false;
    }
    const wholeByteCount = Math.trunc(this.bitSize / 8);
    if (this.bitOffset === 0 && other.bitOffset === 0) {
      for (let i = 0;i < wholeByteCount; i++) {
        if (this.rawBuffer[i] !== other.rawBuffer[i]) {
          return false;
        }
      }
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (this.rawBuffer[wholeByteCount] >> unusedLowBitCount !== other.rawBuffer[wholeByteCount] >> unusedLowBitCount) {
          return false;
        }
      }
    } else {
      for (let i = 0;i < wholeByteCount; i++) {
        const a = bitArrayByteAt(this.rawBuffer, this.bitOffset, i);
        const b = bitArrayByteAt(other.rawBuffer, other.bitOffset, i);
        if (a !== b) {
          return false;
        }
      }
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const a = bitArrayByteAt(this.rawBuffer, this.bitOffset, wholeByteCount);
        const b = bitArrayByteAt(other.rawBuffer, other.bitOffset, wholeByteCount);
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (a >> unusedLowBitCount !== b >> unusedLowBitCount) {
          return false;
        }
      }
    }
    return true;
  }
  get buffer() {
    bitArrayPrintDeprecationWarning("buffer", "Use BitArray.byteAt() or BitArray.rawBuffer instead");
    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error("BitArray.buffer does not support unaligned bit arrays");
    }
    return this.rawBuffer;
  }
  get length() {
    bitArrayPrintDeprecationWarning("length", "Use BitArray.bitSize or BitArray.byteSize instead");
    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error("BitArray.length does not support unaligned bit arrays");
    }
    return this.rawBuffer.length;
  }
}
function bitArrayByteAt(buffer, bitOffset, index) {
  if (bitOffset === 0) {
    return buffer[index] ?? 0;
  } else {
    const a = buffer[index] << bitOffset & 255;
    const b = buffer[index + 1] >> 8 - bitOffset;
    return a | b;
  }
}

class UtfCodepoint {
  constructor(value) {
    this.value = value;
  }
}
var isBitArrayDeprecationMessagePrinted = {};
function bitArrayPrintDeprecationWarning(name, message) {
  if (isBitArrayDeprecationMessagePrinted[name]) {
    return;
  }
  console.warn(`Deprecated BitArray.${name} property used in JavaScript FFI code. ${message}.`);
  isBitArrayDeprecationMessagePrinted[name] = true;
}
class Result extends CustomType {
  static isResult(data) {
    return data instanceof Result;
  }
}

class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }
  isOk() {
    return true;
  }
}

class Error extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  isOk() {
    return false;
  }
}
function isEqual(x, y) {
  let values = [x, y];
  while (values.length) {
    let a = values.pop();
    let b = values.pop();
    if (a === b)
      continue;
    if (!isObject(a) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a, b) || unequalDates(a, b) || unequalBuffers(a, b) || unequalArrays(a, b) || unequalMaps(a, b) || unequalSets(a, b) || unequalRegExps(a, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a.equals(b))
          continue;
        else
          return false;
      } catch {}
    }
    let [keys, get] = getters(a);
    for (let k of keys(a)) {
      values.push(get(a, k), get(b, k));
    }
  }
  return true;
}
function getters(object) {
  if (object instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a, b) {
  return a instanceof Date && (a > b || a < b);
}
function unequalBuffers(a, b) {
  return !(a instanceof BitArray) && a.buffer instanceof ArrayBuffer && a.BYTES_PER_ELEMENT && !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]));
}
function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}
function unequalMaps(a, b) {
  return a instanceof Map && a.size !== b.size;
}
function unequalSets(a, b) {
  return a instanceof Set && (a.size != b.size || [...a].some((e) => !b.has(e)));
}
function unequalRegExps(a, b) {
  return a instanceof RegExp && (a.source !== b.source || a.flags !== b.flags);
}
function isObject(a) {
  return typeof a === "object" && a !== null;
}
function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c))
    return false;
  return a.constructor === b.constructor;
}
function makeError(variant, file, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.file = file;
  error.module = module;
  error.line = line;
  error.function = fn;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}
// build/dev/javascript/gleam_stdlib/gleam/option.mjs
class None extends CustomType {
}

// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap;
var tempDataView = /* @__PURE__ */ new DataView(/* @__PURE__ */ new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== undefined) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0;i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {}
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0;i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys = Object.keys(o);
    for (let i = 0;i < keys.length; i++) {
      const k = keys[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === undefined)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var ENTRY = 0;
var ARRAY_NODE = 1;
var INDEX_NODE = 2;
var COLLISION_NODE = 3;
var EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: []
};
function mask(hash, shift) {
  return hash >>> shift & MASK;
}
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
function bitcount(x) {
  x -= x >> 1 & 1431655765;
  x = (x & 858993459) + (x >> 2 & 858993459);
  x = x + (x >> 4) & 252645135;
  x += x >> 8;
  x += x >> 16;
  return x & 127;
}
function index(bitmap, bit) {
  return bitcount(bitmap & bit - 1);
}
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0;i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 }
      ]
    };
  }
  const addedLeaf = { val: false };
  return assoc(assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf), shift, key2hash, key2, val2, addedLeaf);
}
function assoc(root2, shift, hash, key, val, addedLeaf) {
  switch (root2.type) {
    case ARRAY_NODE:
      return assocArray(root2, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root2, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root2, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root2, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === undefined) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root2.size + 1,
      array: cloneAndSet(root2.array, idx, { type: ENTRY, k: key, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key, node.k)) {
      if (val === node.v) {
        return root2;
      }
      return {
        type: ARRAY_NODE,
        size: root2.size,
        array: cloneAndSet(root2.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root2.size,
      array: cloneAndSet(root2.array, idx, createNode(shift + SHIFT, node.k, node.v, hash, key, val))
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  if (n === node) {
    return root2;
  }
  return {
    type: ARRAY_NODE,
    size: root2.size,
    array: cloneAndSet(root2.array, idx, n)
  };
}
function assocIndex(root2, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root2.bitmap, bit);
  if ((root2.bitmap & bit) !== 0) {
    const node = root2.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root2;
      }
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key, nodeKey)) {
      if (val === node.v) {
        return root2;
      }
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap,
      array: cloneAndSet(root2.array, idx, createNode(shift + SHIFT, nodeKey, node.v, hash, key, val))
    };
  } else {
    const n = root2.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root2.bitmap;
      for (let i = 0;i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root2.array[j++];
          nodes[i] = node;
        }
        bitmap = bitmap >>> 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes
      };
    } else {
      const newArray = spliceIn(root2.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root2, shift, hash, key, val, addedLeaf) {
  if (hash === root2.hash) {
    const idx = collisionIndexOf(root2, key);
    if (idx !== -1) {
      const entry = root2.array[idx];
      if (entry.v === val) {
        return root2;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root2.array, idx, { type: ENTRY, k: key, v: val })
      };
    }
    const size = root2.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root2.array, size, { type: ENTRY, k: key, v: val })
    };
  }
  return assoc({
    type: INDEX_NODE,
    bitmap: bitpos(root2.hash, shift),
    array: [root2]
  }, shift, hash, key, val, addedLeaf);
}
function collisionIndexOf(root2, key) {
  const size = root2.array.length;
  for (let i = 0;i < size; i++) {
    if (isEqual(key, root2.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root2, shift, hash, key) {
  switch (root2.type) {
    case ARRAY_NODE:
      return findArray(root2, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root2, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root2, key);
  }
}
function findArray(root2, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === undefined) {
    return;
  }
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return;
}
function findIndex(root2, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root2.bitmap & bit) === 0) {
    return;
  }
  const idx = index(root2.bitmap, bit);
  const node = root2.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return;
}
function findCollision(root2, key) {
  const idx = collisionIndexOf(root2, key);
  if (idx < 0) {
    return;
  }
  return root2.array[idx];
}
function without(root2, shift, hash, key) {
  switch (root2.type) {
    case ARRAY_NODE:
      return withoutArray(root2, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root2, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root2, key);
  }
}
function withoutArray(root2, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === undefined) {
    return root2;
  }
  let n = undefined;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key)) {
      return root2;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root2;
    }
  }
  if (n === undefined) {
    if (root2.size <= MIN_ARRAY_NODE) {
      const arr = root2.array;
      const out = new Array(root2.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== undefined) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i;
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== undefined) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap,
        array: out
      };
    }
    return {
      type: ARRAY_NODE,
      size: root2.size - 1,
      array: cloneAndSet(root2.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root2.size,
    array: cloneAndSet(root2.array, idx, n)
  };
}
function withoutIndex(root2, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root2.bitmap & bit) === 0) {
    return root2;
  }
  const idx = index(root2.bitmap, bit);
  const node = root2.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root2;
    }
    if (n !== undefined) {
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, n)
      };
    }
    if (root2.bitmap === bit) {
      return;
    }
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap ^ bit,
      array: spliceOut(root2.array, idx)
    };
  }
  if (isEqual(key, node.k)) {
    if (root2.bitmap === bit) {
      return;
    }
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap ^ bit,
      array: spliceOut(root2.array, idx)
    };
  }
  return root2;
}
function withoutCollision(root2, key) {
  const idx = collisionIndexOf(root2, key);
  if (idx < 0) {
    return root2;
  }
  if (root2.array.length === 1) {
    return;
  }
  return {
    type: COLLISION_NODE,
    hash: root2.hash,
    array: spliceOut(root2.array, idx)
  };
}
function forEach(root2, fn) {
  if (root2 === undefined) {
    return;
  }
  const items = root2.array;
  const size = items.length;
  for (let i = 0;i < size; i++) {
    const item = items[i];
    if (item === undefined) {
      continue;
    }
    if (item.type === ENTRY) {
      fn(item.v, item.k);
      continue;
    }
    forEach(item, fn);
  }
}

class Dict {
  static fromObject(o) {
    const keys = Object.keys(o);
    let m = Dict.new();
    for (let i = 0;i < keys.length; i++) {
      const k = keys[i];
      m = m.set(k, o[k]);
    }
    return m;
  }
  static fromMap(o) {
    let m = Dict.new();
    o.forEach((v, k) => {
      m = m.set(k, v);
    });
    return m;
  }
  static new() {
    return new Dict(undefined, 0);
  }
  constructor(root2, size) {
    this.root = root2;
    this.size = size;
  }
  get(key, notFound) {
    if (this.root === undefined) {
      return notFound;
    }
    const found = find(this.root, 0, getHash(key), key);
    if (found === undefined) {
      return notFound;
    }
    return found.v;
  }
  set(key, val) {
    const addedLeaf = { val: false };
    const root2 = this.root === undefined ? EMPTY : this.root;
    const newRoot = assoc(root2, 0, getHash(key), key, val, addedLeaf);
    if (newRoot === this.root) {
      return this;
    }
    return new Dict(newRoot, addedLeaf.val ? this.size + 1 : this.size);
  }
  delete(key) {
    if (this.root === undefined) {
      return this;
    }
    const newRoot = without(this.root, 0, getHash(key), key);
    if (newRoot === this.root) {
      return this;
    }
    if (newRoot === undefined) {
      return Dict.new();
    }
    return new Dict(newRoot, this.size - 1);
  }
  has(key) {
    if (this.root === undefined) {
      return false;
    }
    return find(this.root, 0, getHash(key), key) !== undefined;
  }
  entries() {
    if (this.root === undefined) {
      return [];
    }
    const result = [];
    this.forEach((v, k) => result.push([k, v]));
    return result;
  }
  forEach(fn) {
    forEach(this.root, fn);
  }
  hashCode() {
    let h = 0;
    this.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
    return h;
  }
  equals(o) {
    if (!(o instanceof Dict) || this.size !== o.size) {
      return false;
    }
    try {
      this.forEach((v, k) => {
        if (!isEqual(o.get(k, !v), v)) {
          throw unequalDictSymbol;
        }
      });
      return true;
    } catch (e) {
      if (e === unequalDictSymbol) {
        return false;
      }
      throw e;
    }
  }
}
var unequalDictSymbol = /* @__PURE__ */ Symbol();

// build/dev/javascript/gleam_stdlib/gleam/order.mjs
class Lt extends CustomType {
}

class Eq extends CustomType {
}

class Gt extends CustomType {
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
class Ascending extends CustomType {
}

class Descending extends CustomType {
}
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix instanceof Empty) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function reverse(list) {
  return reverse_and_prepend(list, toList([]));
}
function append_loop(loop$first, loop$second) {
  while (true) {
    let first = loop$first;
    let second = loop$second;
    if (first instanceof Empty) {
      return second;
    } else {
      let first$1 = first.head;
      let rest$1 = first.tail;
      loop$first = rest$1;
      loop$second = prepend(first$1, second);
    }
  }
}
function append(first, second) {
  return append_loop(reverse(first), second);
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list instanceof Empty) {
      return initial;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, first$1);
      loop$fun = fun;
    }
  }
}
function sequences(loop$list, loop$compare, loop$growing, loop$direction, loop$prev, loop$acc) {
  while (true) {
    let list = loop$list;
    let compare3 = loop$compare;
    let growing = loop$growing;
    let direction = loop$direction;
    let prev = loop$prev;
    let acc = loop$acc;
    let growing$1 = prepend(prev, growing);
    if (list instanceof Empty) {
      if (direction instanceof Ascending) {
        return prepend(reverse(growing$1), acc);
      } else {
        return prepend(growing$1, acc);
      }
    } else {
      let new$1 = list.head;
      let rest$1 = list.tail;
      let $ = compare3(prev, new$1);
      if (direction instanceof Ascending) {
        if ($ instanceof Lt) {
          loop$list = rest$1;
          loop$compare = compare3;
          loop$growing = growing$1;
          loop$direction = direction;
          loop$prev = new$1;
          loop$acc = acc;
        } else if ($ instanceof Eq) {
          loop$list = rest$1;
          loop$compare = compare3;
          loop$growing = growing$1;
          loop$direction = direction;
          loop$prev = new$1;
          loop$acc = acc;
        } else {
          let _block;
          if (direction instanceof Ascending) {
            _block = prepend(reverse(growing$1), acc);
          } else {
            _block = prepend(growing$1, acc);
          }
          let acc$1 = _block;
          if (rest$1 instanceof Empty) {
            return prepend(toList([new$1]), acc$1);
          } else {
            let next = rest$1.head;
            let rest$2 = rest$1.tail;
            let _block$1;
            let $1 = compare3(new$1, next);
            if ($1 instanceof Lt) {
              _block$1 = new Ascending;
            } else if ($1 instanceof Eq) {
              _block$1 = new Ascending;
            } else {
              _block$1 = new Descending;
            }
            let direction$1 = _block$1;
            loop$list = rest$2;
            loop$compare = compare3;
            loop$growing = toList([new$1]);
            loop$direction = direction$1;
            loop$prev = next;
            loop$acc = acc$1;
          }
        }
      } else if ($ instanceof Lt) {
        let _block;
        if (direction instanceof Ascending) {
          _block = prepend(reverse(growing$1), acc);
        } else {
          _block = prepend(growing$1, acc);
        }
        let acc$1 = _block;
        if (rest$1 instanceof Empty) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let _block$1;
          let $1 = compare3(new$1, next);
          if ($1 instanceof Lt) {
            _block$1 = new Ascending;
          } else if ($1 instanceof Eq) {
            _block$1 = new Ascending;
          } else {
            _block$1 = new Descending;
          }
          let direction$1 = _block$1;
          loop$list = rest$2;
          loop$compare = compare3;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else if ($ instanceof Eq) {
        let _block;
        if (direction instanceof Ascending) {
          _block = prepend(reverse(growing$1), acc);
        } else {
          _block = prepend(growing$1, acc);
        }
        let acc$1 = _block;
        if (rest$1 instanceof Empty) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let _block$1;
          let $1 = compare3(new$1, next);
          if ($1 instanceof Lt) {
            _block$1 = new Ascending;
          } else if ($1 instanceof Eq) {
            _block$1 = new Ascending;
          } else {
            _block$1 = new Descending;
          }
          let direction$1 = _block$1;
          loop$list = rest$2;
          loop$compare = compare3;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else {
        loop$list = rest$1;
        loop$compare = compare3;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      }
    }
  }
}
function merge_ascendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare3 = loop$compare;
    let acc = loop$acc;
    if (list1 instanceof Empty) {
      let list = list2;
      return reverse_and_prepend(list, acc);
    } else if (list2 instanceof Empty) {
      let list = list1;
      return reverse_and_prepend(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare3(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare3;
        loop$acc = prepend(first1, acc);
      } else if ($ instanceof Eq) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare3;
        loop$acc = prepend(first2, acc);
      } else {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare3;
        loop$acc = prepend(first2, acc);
      }
    }
  }
}
function merge_ascending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences2 = loop$sequences;
    let compare3 = loop$compare;
    let acc = loop$acc;
    if (sequences2 instanceof Empty) {
      return reverse(acc);
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(prepend(reverse(sequence), acc));
      } else {
        let ascending1 = sequences2.head;
        let ascending2 = $.head;
        let rest$1 = $.tail;
        let descending = merge_ascendings(ascending1, ascending2, compare3, toList([]));
        loop$sequences = rest$1;
        loop$compare = compare3;
        loop$acc = prepend(descending, acc);
      }
    }
  }
}
function merge_descendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare3 = loop$compare;
    let acc = loop$acc;
    if (list1 instanceof Empty) {
      let list = list2;
      return reverse_and_prepend(list, acc);
    } else if (list2 instanceof Empty) {
      let list = list1;
      return reverse_and_prepend(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare3(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare3;
        loop$acc = prepend(first2, acc);
      } else if ($ instanceof Eq) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare3;
        loop$acc = prepend(first1, acc);
      } else {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare3;
        loop$acc = prepend(first1, acc);
      }
    }
  }
}
function merge_descending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences2 = loop$sequences;
    let compare3 = loop$compare;
    let acc = loop$acc;
    if (sequences2 instanceof Empty) {
      return reverse(acc);
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(prepend(reverse(sequence), acc));
      } else {
        let descending1 = sequences2.head;
        let descending2 = $.head;
        let rest$1 = $.tail;
        let ascending = merge_descendings(descending1, descending2, compare3, toList([]));
        loop$sequences = rest$1;
        loop$compare = compare3;
        loop$acc = prepend(ascending, acc);
      }
    }
  }
}
function merge_all(loop$sequences, loop$direction, loop$compare) {
  while (true) {
    let sequences2 = loop$sequences;
    let direction = loop$direction;
    let compare3 = loop$compare;
    if (sequences2 instanceof Empty) {
      return toList([]);
    } else if (direction instanceof Ascending) {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return sequence;
      } else {
        let sequences$1 = merge_ascending_pairs(sequences2, compare3, toList([]));
        loop$sequences = sequences$1;
        loop$direction = new Descending;
        loop$compare = compare3;
      }
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(sequence);
      } else {
        let sequences$1 = merge_descending_pairs(sequences2, compare3, toList([]));
        loop$sequences = sequences$1;
        loop$direction = new Ascending;
        loop$compare = compare3;
      }
    }
  }
}
function sort(list, compare3) {
  if (list instanceof Empty) {
    return toList([]);
  } else {
    let $ = list.tail;
    if ($ instanceof Empty) {
      let x = list.head;
      return toList([x]);
    } else {
      let x = list.head;
      let y = $.head;
      let rest$1 = $.tail;
      let _block;
      let $1 = compare3(x, y);
      if ($1 instanceof Lt) {
        _block = new Ascending;
      } else if ($1 instanceof Eq) {
        _block = new Ascending;
      } else {
        _block = new Descending;
      }
      let direction = _block;
      let sequences$1 = sequences(rest$1, compare3, toList([x]), direction, y, toList([]));
      return merge_all(sequences$1, new Ascending, compare3);
    }
  }
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function concat_loop(loop$strings, loop$accumulator) {
  while (true) {
    let strings = loop$strings;
    let accumulator = loop$accumulator;
    if (strings instanceof Empty) {
      return accumulator;
    } else {
      let string = strings.head;
      let strings$1 = strings.tail;
      loop$strings = strings$1;
      loop$accumulator = accumulator + string;
    }
  }
}
function concat2(strings) {
  return concat_loop(strings, "");
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic/decode.mjs
class Decoder extends CustomType {
  constructor(function$) {
    super();
    this.function = function$;
  }
}
function run(data, decoder) {
  let $ = decoder.function(data);
  let maybe_invalid_data = $[0];
  let errors = $[1];
  if (errors instanceof Empty) {
    return new Ok(maybe_invalid_data);
  } else {
    return new Error(errors);
  }
}
function map2(decoder, transformer) {
  return new Decoder((d) => {
    let $ = decoder.function(d);
    let data = $[0];
    let errors = $[1];
    return [transformer(data), errors];
  });
}

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = undefined;
var NOT_FOUND = {};
function to_string(term) {
  return term.toString();
}
function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
}
var unicode_whitespaces = [
  " ",
  "\t",
  `
`,
  "\v",
  "\f",
  "\r",
  "",
  "\u2028",
  "\u2029"
].join("");
var trim_start_regex = /* @__PURE__ */ new RegExp(`^[${unicode_whitespaces}]*`);
var trim_end_regex = /* @__PURE__ */ new RegExp(`[${unicode_whitespaces}]*$`);
function new_map() {
  return Dict.new();
}
function map_get(map3, key) {
  const value = map3.get(key, NOT_FOUND);
  if (value === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value);
}
function map_insert(key, value, map3) {
  return map3.set(key, value);
}
function float_to_string(float2) {
  const string2 = float2.toString().replace("+", "");
  if (string2.indexOf(".") >= 0) {
    return string2;
  } else {
    const index3 = string2.indexOf("e");
    if (index3 >= 0) {
      return string2.slice(0, index3) + ".0" + string2.slice(index3);
    } else {
      return string2 + ".0";
    }
  }
}

class Inspector {
  #references = new Set;
  inspect(v) {
    const t = typeof v;
    if (v === true)
      return "True";
    if (v === false)
      return "False";
    if (v === null)
      return "//js(null)";
    if (v === undefined)
      return "Nil";
    if (t === "string")
      return this.#string(v);
    if (t === "bigint" || Number.isInteger(v))
      return v.toString();
    if (t === "number")
      return float_to_string(v);
    if (v instanceof UtfCodepoint)
      return this.#utfCodepoint(v);
    if (v instanceof BitArray)
      return this.#bit_array(v);
    if (v instanceof RegExp)
      return `//js(${v})`;
    if (v instanceof Date)
      return `//js(Date("${v.toISOString()}"))`;
    if (v instanceof globalThis.Error)
      return `//js(${v.toString()})`;
    if (v instanceof Function) {
      const args = [];
      for (const i of Array(v.length).keys())
        args.push(String.fromCharCode(i + 97));
      return `//fn(${args.join(", ")}) { ... }`;
    }
    if (this.#references.size === this.#references.add(v).size) {
      return "//js(circular reference)";
    }
    let printed;
    if (Array.isArray(v)) {
      printed = `#(${v.map((v2) => this.inspect(v2)).join(", ")})`;
    } else if (v instanceof List) {
      printed = this.#list(v);
    } else if (v instanceof CustomType) {
      printed = this.#customType(v);
    } else if (v instanceof Dict) {
      printed = this.#dict(v);
    } else if (v instanceof Set) {
      return `//js(Set(${[...v].map((v2) => this.inspect(v2)).join(", ")}))`;
    } else {
      printed = this.#object(v);
    }
    this.#references.delete(v);
    return printed;
  }
  #object(v) {
    const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
    const props = [];
    for (const k of Object.keys(v)) {
      props.push(`${this.inspect(k)}: ${this.inspect(v[k])}`);
    }
    const body = props.length ? " " + props.join(", ") + " " : "";
    const head = name === "Object" ? "" : name + " ";
    return `//js(${head}{${body}})`;
  }
  #dict(map3) {
    let body = "dict.from_list([";
    let first = true;
    map3.forEach((value, key) => {
      if (!first)
        body = body + ", ";
      body = body + "#(" + this.inspect(key) + ", " + this.inspect(value) + ")";
      first = false;
    });
    return body + "])";
  }
  #customType(record) {
    const props = Object.keys(record).map((label) => {
      const value = this.inspect(record[label]);
      return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
    }).join(", ");
    return props ? `${record.constructor.name}(${props})` : record.constructor.name;
  }
  #list(list2) {
    if (list2 instanceof Empty) {
      return "[]";
    }
    let char_out = 'charlist.from_string("';
    let list_out = "[";
    let current = list2;
    while (current instanceof NonEmpty) {
      let element = current.head;
      current = current.tail;
      if (list_out !== "[") {
        list_out += ", ";
      }
      list_out += this.inspect(element);
      if (char_out) {
        if (Number.isInteger(element) && element >= 32 && element <= 126) {
          char_out += String.fromCharCode(element);
        } else {
          char_out = null;
        }
      }
    }
    if (char_out) {
      return char_out + '")';
    } else {
      return list_out + "]";
    }
  }
  #string(str) {
    let new_str = '"';
    for (let i = 0;i < str.length; i++) {
      const char = str[i];
      switch (char) {
        case `
`:
          new_str += "\\n";
          break;
        case "\r":
          new_str += "\\r";
          break;
        case "\t":
          new_str += "\\t";
          break;
        case "\f":
          new_str += "\\f";
          break;
        case "\\":
          new_str += "\\\\";
          break;
        case '"':
          new_str += "\\\"";
          break;
        default:
          if (char < " " || char > "~" && char < " ") {
            new_str += "\\u{" + char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") + "}";
          } else {
            new_str += char;
          }
      }
    }
    new_str += '"';
    return new_str;
  }
  #utfCodepoint(codepoint2) {
    return `//utfcodepoint(${String.fromCodePoint(codepoint2.value)})`;
  }
  #bit_array(bits) {
    if (bits.bitSize === 0) {
      return "<<>>";
    }
    let acc = "<<";
    for (let i = 0;i < bits.byteSize - 1; i++) {
      acc += bits.byteAt(i).toString();
      acc += ", ";
    }
    if (bits.byteSize * 8 === bits.bitSize) {
      acc += bits.byteAt(bits.byteSize - 1).toString();
    } else {
      const trailingBitsCount = bits.bitSize % 8;
      acc += bits.byteAt(bits.byteSize - 1) >> 8 - trailingBitsCount;
      acc += `:size(${trailingBitsCount})`;
    }
    acc += ">>";
    return acc;
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function insert(dict2, key, value) {
  return map_insert(key, value, dict2);
}
// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function is_ok(result) {
  if (result instanceof Ok) {
    return true;
  } else {
    return false;
  }
}
// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/function.mjs
function identity2(x) {
  return x;
}
// build/dev/javascript/gleam_stdlib/gleam/set.mjs
class Set2 extends CustomType {
  constructor(dict2) {
    super();
    this.dict = dict2;
  }
}
function new$() {
  return new Set2(new_map());
}
function contains(set, member) {
  let _pipe = set.dict;
  let _pipe$1 = map_get(_pipe, member);
  return is_ok(_pipe$1);
}
var token = undefined;
function insert2(set, member) {
  return new Set2(insert(set.dict, member, token));
}

// build/dev/javascript/lustre/lustre/internals/constants.ffi.mjs
var EMPTY_DICT = /* @__PURE__ */ Dict.new();
var EMPTY_SET = /* @__PURE__ */ new$();
var empty_dict = () => EMPTY_DICT;
var empty_set = () => EMPTY_SET;
var document = () => globalThis?.document;
var NAMESPACE_HTML = "http://www.w3.org/1999/xhtml";
var ELEMENT_NODE = 1;
var TEXT_NODE = 3;
var DOCUMENT_FRAGMENT_NODE = 11;
var SUPPORTS_MOVE_BEFORE = !!globalThis.HTMLElement?.prototype?.moveBefore;

// build/dev/javascript/lustre/lustre/internals/constants.mjs
var empty_list = /* @__PURE__ */ toList([]);
var option_none = /* @__PURE__ */ new None;

// build/dev/javascript/lustre/lustre/vdom/vattr.ffi.mjs
var GT = /* @__PURE__ */ new Gt;
var LT = /* @__PURE__ */ new Lt;
var EQ = /* @__PURE__ */ new Eq;
function compare3(a, b) {
  if (a.name === b.name) {
    return EQ;
  } else if (a.name < b.name) {
    return LT;
  } else {
    return GT;
  }
}

// build/dev/javascript/lustre/lustre/vdom/vattr.mjs
class Attribute extends CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}

class Property extends CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}

class Event2 extends CustomType {
  constructor(kind, name, handler, include, prevent_default, stop_propagation, immediate2, debounce, throttle) {
    super();
    this.kind = kind;
    this.name = name;
    this.handler = handler;
    this.include = include;
    this.prevent_default = prevent_default;
    this.stop_propagation = stop_propagation;
    this.immediate = immediate2;
    this.debounce = debounce;
    this.throttle = throttle;
  }
}

class Handler extends CustomType {
  constructor(prevent_default, stop_propagation, message) {
    super();
    this.prevent_default = prevent_default;
    this.stop_propagation = stop_propagation;
    this.message = message;
  }
}
function merge(loop$attributes, loop$merged) {
  while (true) {
    let attributes = loop$attributes;
    let merged = loop$merged;
    if (attributes instanceof Empty) {
      return merged;
    } else {
      let $ = attributes.head;
      if ($ instanceof Attribute) {
        let $1 = $.name;
        if ($1 === "") {
          let rest = attributes.tail;
          loop$attributes = rest;
          loop$merged = merged;
        } else if ($1 === "class") {
          let $2 = $.value;
          if ($2 === "") {
            let rest = attributes.tail;
            loop$attributes = rest;
            loop$merged = merged;
          } else {
            let $3 = attributes.tail;
            if ($3 instanceof Empty) {
              let attribute$1 = $;
              let rest = $3;
              loop$attributes = rest;
              loop$merged = prepend(attribute$1, merged);
            } else {
              let $4 = $3.head;
              if ($4 instanceof Attribute) {
                let $5 = $4.name;
                if ($5 === "class") {
                  let kind = $.kind;
                  let class1 = $2;
                  let rest = $3.tail;
                  let class2 = $4.value;
                  let value = class1 + " " + class2;
                  let attribute$1 = new Attribute(kind, "class", value);
                  loop$attributes = prepend(attribute$1, rest);
                  loop$merged = merged;
                } else {
                  let attribute$1 = $;
                  let rest = $3;
                  loop$attributes = rest;
                  loop$merged = prepend(attribute$1, merged);
                }
              } else {
                let attribute$1 = $;
                let rest = $3;
                loop$attributes = rest;
                loop$merged = prepend(attribute$1, merged);
              }
            }
          }
        } else if ($1 === "style") {
          let $2 = $.value;
          if ($2 === "") {
            let rest = attributes.tail;
            loop$attributes = rest;
            loop$merged = merged;
          } else {
            let $3 = attributes.tail;
            if ($3 instanceof Empty) {
              let attribute$1 = $;
              let rest = $3;
              loop$attributes = rest;
              loop$merged = prepend(attribute$1, merged);
            } else {
              let $4 = $3.head;
              if ($4 instanceof Attribute) {
                let $5 = $4.name;
                if ($5 === "style") {
                  let kind = $.kind;
                  let style1 = $2;
                  let rest = $3.tail;
                  let style2 = $4.value;
                  let value = style1 + ";" + style2;
                  let attribute$1 = new Attribute(kind, "style", value);
                  loop$attributes = prepend(attribute$1, rest);
                  loop$merged = merged;
                } else {
                  let attribute$1 = $;
                  let rest = $3;
                  loop$attributes = rest;
                  loop$merged = prepend(attribute$1, merged);
                }
              } else {
                let attribute$1 = $;
                let rest = $3;
                loop$attributes = rest;
                loop$merged = prepend(attribute$1, merged);
              }
            }
          }
        } else {
          let attribute$1 = $;
          let rest = attributes.tail;
          loop$attributes = rest;
          loop$merged = prepend(attribute$1, merged);
        }
      } else {
        let attribute$1 = $;
        let rest = attributes.tail;
        loop$attributes = rest;
        loop$merged = prepend(attribute$1, merged);
      }
    }
  }
}
function prepare(attributes) {
  if (attributes instanceof Empty) {
    return attributes;
  } else {
    let $ = attributes.tail;
    if ($ instanceof Empty) {
      return attributes;
    } else {
      let _pipe = attributes;
      let _pipe$1 = sort(_pipe, (a, b) => {
        return compare3(b, a);
      });
      return merge(_pipe$1, empty_list);
    }
  }
}
var attribute_kind = 0;
function attribute(name, value) {
  return new Attribute(attribute_kind, name, value);
}
var property_kind = 1;
var event_kind = 2;
var never_kind = 0;
var always_kind = 2;

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute2(name, value) {
  return attribute(name, value);
}

// build/dev/javascript/lustre/lustre/effect.mjs
class Effect extends CustomType {
  constructor(synchronous, before_paint, after_paint) {
    super();
    this.synchronous = synchronous;
    this.before_paint = before_paint;
    this.after_paint = after_paint;
  }
}
var empty = /* @__PURE__ */ new Effect(/* @__PURE__ */ toList([]), /* @__PURE__ */ toList([]), /* @__PURE__ */ toList([]));
function none() {
  return empty;
}

// build/dev/javascript/lustre/lustre/internals/mutable_map.ffi.mjs
function empty2() {
  return null;
}
function get(map3, key) {
  const value = map3?.get(key);
  if (value != null) {
    return new Ok(value);
  } else {
    return new Error(undefined);
  }
}
function insert3(map3, key, value) {
  map3 ??= new Map;
  map3.set(key, value);
  return map3;
}
function remove(map3, key) {
  map3?.delete(key);
  return map3;
}

// build/dev/javascript/lustre/lustre/vdom/path.mjs
class Root extends CustomType {
}

class Key extends CustomType {
  constructor(key, parent) {
    super();
    this.key = key;
    this.parent = parent;
  }
}

class Index extends CustomType {
  constructor(index3, parent) {
    super();
    this.index = index3;
    this.parent = parent;
  }
}
function do_matches(loop$path, loop$candidates) {
  while (true) {
    let path = loop$path;
    let candidates = loop$candidates;
    if (candidates instanceof Empty) {
      return false;
    } else {
      let candidate = candidates.head;
      let rest = candidates.tail;
      let $ = starts_with(path, candidate);
      if ($) {
        return true;
      } else {
        loop$path = path;
        loop$candidates = rest;
      }
    }
  }
}
function add2(parent, index3, key) {
  if (key === "") {
    return new Index(index3, parent);
  } else {
    return new Key(key, parent);
  }
}
var root2 = /* @__PURE__ */ new Root;
var separator_element = "\t";
function do_to_string(loop$path, loop$acc) {
  while (true) {
    let path = loop$path;
    let acc = loop$acc;
    if (path instanceof Root) {
      if (acc instanceof Empty) {
        return "";
      } else {
        let segments = acc.tail;
        return concat2(segments);
      }
    } else if (path instanceof Key) {
      let key = path.key;
      let parent = path.parent;
      loop$path = parent;
      loop$acc = prepend(separator_element, prepend(key, acc));
    } else {
      let index3 = path.index;
      let parent = path.parent;
      loop$path = parent;
      loop$acc = prepend(separator_element, prepend(to_string(index3), acc));
    }
  }
}
function to_string2(path) {
  return do_to_string(path, toList([]));
}
function matches(path, candidates) {
  if (candidates instanceof Empty) {
    return false;
  } else {
    return do_matches(to_string2(path), candidates);
  }
}
var separator_event = `
`;
function event(path, event2) {
  return do_to_string(path, toList([separator_event, event2]));
}

// build/dev/javascript/lustre/lustre/vdom/vnode.mjs
class Fragment extends CustomType {
  constructor(kind, key, mapper, children, keyed_children, children_count) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.children = children;
    this.keyed_children = keyed_children;
    this.children_count = children_count;
  }
}

class Element extends CustomType {
  constructor(kind, key, mapper, namespace, tag, attributes, children, keyed_children, self_closing, void$) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.children = children;
    this.keyed_children = keyed_children;
    this.self_closing = self_closing;
    this.void = void$;
  }
}

class Text extends CustomType {
  constructor(kind, key, mapper, content) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.content = content;
  }
}

class UnsafeInnerHtml extends CustomType {
  constructor(kind, key, mapper, namespace, tag, attributes, inner_html) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.inner_html = inner_html;
  }
}
function is_void_element(tag, namespace) {
  if (namespace === "") {
    if (tag === "area") {
      return true;
    } else if (tag === "base") {
      return true;
    } else if (tag === "br") {
      return true;
    } else if (tag === "col") {
      return true;
    } else if (tag === "embed") {
      return true;
    } else if (tag === "hr") {
      return true;
    } else if (tag === "img") {
      return true;
    } else if (tag === "input") {
      return true;
    } else if (tag === "link") {
      return true;
    } else if (tag === "meta") {
      return true;
    } else if (tag === "param") {
      return true;
    } else if (tag === "source") {
      return true;
    } else if (tag === "track") {
      return true;
    } else if (tag === "wbr") {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}
function advance(node) {
  if (node instanceof Fragment) {
    let children_count = node.children_count;
    return 1 + children_count;
  } else {
    return 1;
  }
}
var fragment_kind = 0;
function fragment(key, mapper, children, keyed_children, children_count) {
  return new Fragment(fragment_kind, key, mapper, children, keyed_children, children_count);
}
var element_kind = 1;
function element(key, mapper, namespace, tag, attributes, children, keyed_children, self_closing, void$) {
  return new Element(element_kind, key, mapper, namespace, tag, prepare(attributes), children, keyed_children, self_closing, void$ || is_void_element(tag, namespace));
}
var text_kind = 2;
function text(key, mapper, content) {
  return new Text(text_kind, key, mapper, content);
}
var unsafe_inner_html_kind = 3;
function set_fragment_key(loop$key, loop$children, loop$index, loop$new_children, loop$keyed_children) {
  while (true) {
    let key = loop$key;
    let children = loop$children;
    let index3 = loop$index;
    let new_children = loop$new_children;
    let keyed_children = loop$keyed_children;
    if (children instanceof Empty) {
      return [reverse(new_children), keyed_children];
    } else {
      let $ = children.head;
      if ($ instanceof Fragment) {
        let node = $;
        if (node.key === "") {
          let children$1 = children.tail;
          let child_key = key + "::" + to_string(index3);
          let $1 = set_fragment_key(child_key, node.children, 0, empty_list, empty2());
          let node_children = $1[0];
          let node_keyed_children = $1[1];
          let _block;
          let _record = node;
          _block = new Fragment(_record.kind, _record.key, _record.mapper, node_children, node_keyed_children, _record.children_count);
          let new_node = _block;
          let new_children$1 = prepend(new_node, new_children);
          let index$1 = index3 + 1;
          loop$key = key;
          loop$children = children$1;
          loop$index = index$1;
          loop$new_children = new_children$1;
          loop$keyed_children = keyed_children;
        } else {
          let node$1 = $;
          if (node$1.key !== "") {
            let children$1 = children.tail;
            let child_key = key + "::" + node$1.key;
            let keyed_node = to_keyed(child_key, node$1);
            let new_children$1 = prepend(keyed_node, new_children);
            let keyed_children$1 = insert3(keyed_children, child_key, keyed_node);
            let index$1 = index3 + 1;
            loop$key = key;
            loop$children = children$1;
            loop$index = index$1;
            loop$new_children = new_children$1;
            loop$keyed_children = keyed_children$1;
          } else {
            let node$2 = $;
            let children$1 = children.tail;
            let new_children$1 = prepend(node$2, new_children);
            let index$1 = index3 + 1;
            loop$key = key;
            loop$children = children$1;
            loop$index = index$1;
            loop$new_children = new_children$1;
            loop$keyed_children = keyed_children;
          }
        }
      } else {
        let node = $;
        if (node.key !== "") {
          let children$1 = children.tail;
          let child_key = key + "::" + node.key;
          let keyed_node = to_keyed(child_key, node);
          let new_children$1 = prepend(keyed_node, new_children);
          let keyed_children$1 = insert3(keyed_children, child_key, keyed_node);
          let index$1 = index3 + 1;
          loop$key = key;
          loop$children = children$1;
          loop$index = index$1;
          loop$new_children = new_children$1;
          loop$keyed_children = keyed_children$1;
        } else {
          let node$1 = $;
          let children$1 = children.tail;
          let new_children$1 = prepend(node$1, new_children);
          let index$1 = index3 + 1;
          loop$key = key;
          loop$children = children$1;
          loop$index = index$1;
          loop$new_children = new_children$1;
          loop$keyed_children = keyed_children;
        }
      }
    }
  }
}
function to_keyed(key, node) {
  if (node instanceof Fragment) {
    let children = node.children;
    let $ = set_fragment_key(key, children, 0, empty_list, empty2());
    let children$1 = $[0];
    let keyed_children = $[1];
    let _record = node;
    return new Fragment(_record.kind, key, _record.mapper, children$1, keyed_children, _record.children_count);
  } else if (node instanceof Element) {
    let _record = node;
    return new Element(_record.kind, key, _record.mapper, _record.namespace, _record.tag, _record.attributes, _record.children, _record.keyed_children, _record.self_closing, _record.void);
  } else if (node instanceof Text) {
    let _record = node;
    return new Text(_record.kind, key, _record.mapper, _record.content);
  } else {
    let _record = node;
    return new UnsafeInnerHtml(_record.kind, key, _record.mapper, _record.namespace, _record.tag, _record.attributes, _record.inner_html);
  }
}

// build/dev/javascript/lustre/lustre/internals/equals.ffi.mjs
var isReferenceEqual = (a, b) => a === b;
var isEqual2 = (a, b) => {
  if (a === b) {
    return true;
  }
  if (a == null || b == null) {
    return false;
  }
  const type = typeof a;
  if (type !== typeof b) {
    return false;
  }
  if (type !== "object") {
    return false;
  }
  const ctor = a.constructor;
  if (ctor !== b.constructor) {
    return false;
  }
  if (Array.isArray(a)) {
    return areArraysEqual(a, b);
  }
  return areObjectsEqual(a, b);
};
var areArraysEqual = (a, b) => {
  let index3 = a.length;
  if (index3 !== b.length) {
    return false;
  }
  while (index3--) {
    if (!isEqual2(a[index3], b[index3])) {
      return false;
    }
  }
  return true;
};
var areObjectsEqual = (a, b) => {
  const properties = Object.keys(a);
  let index3 = properties.length;
  if (Object.keys(b).length !== index3) {
    return false;
  }
  while (index3--) {
    const property2 = properties[index3];
    if (!Object.hasOwn(b, property2)) {
      return false;
    }
    if (!isEqual2(a[property2], b[property2])) {
      return false;
    }
  }
  return true;
};

// build/dev/javascript/lustre/lustre/vdom/events.mjs
class Events extends CustomType {
  constructor(handlers, dispatched_paths, next_dispatched_paths) {
    super();
    this.handlers = handlers;
    this.dispatched_paths = dispatched_paths;
    this.next_dispatched_paths = next_dispatched_paths;
  }
}
function new$3() {
  return new Events(empty2(), empty_list, empty_list);
}
function tick(events) {
  return new Events(events.handlers, events.next_dispatched_paths, empty_list);
}
function do_remove_event(handlers, path, name) {
  return remove(handlers, event(path, name));
}
function remove_event(events, path, name) {
  let handlers = do_remove_event(events.handlers, path, name);
  let _record = events;
  return new Events(handlers, _record.dispatched_paths, _record.next_dispatched_paths);
}
function remove_attributes(handlers, path, attributes) {
  return fold(attributes, handlers, (events, attribute3) => {
    if (attribute3 instanceof Event2) {
      let name = attribute3.name;
      return do_remove_event(events, path, name);
    } else {
      return events;
    }
  });
}
function handle(events, path, name, event2) {
  let next_dispatched_paths = prepend(path, events.next_dispatched_paths);
  let _block;
  let _record = events;
  _block = new Events(_record.handlers, _record.dispatched_paths, next_dispatched_paths);
  let events$1 = _block;
  let $ = get(events$1.handlers, path + separator_event + name);
  if ($ instanceof Ok) {
    let handler = $[0];
    return [events$1, run(event2, handler)];
  } else {
    return [events$1, new Error(toList([]))];
  }
}
function has_dispatched_events(events, path) {
  return matches(path, events.dispatched_paths);
}
function do_add_event(handlers, mapper, path, name, handler) {
  return insert3(handlers, event(path, name), map2(handler, (handler2) => {
    let _record = handler2;
    return new Handler(_record.prevent_default, _record.stop_propagation, identity2(mapper)(handler2.message));
  }));
}
function add_event(events, mapper, path, name, handler) {
  let handlers = do_add_event(events.handlers, mapper, path, name, handler);
  let _record = events;
  return new Events(handlers, _record.dispatched_paths, _record.next_dispatched_paths);
}
function add_attributes(handlers, mapper, path, attributes) {
  return fold(attributes, handlers, (events, attribute3) => {
    if (attribute3 instanceof Event2) {
      let name = attribute3.name;
      let handler = attribute3.handler;
      return do_add_event(events, mapper, path, name, handler);
    } else {
      return events;
    }
  });
}
function compose_mapper(mapper, child_mapper) {
  let $ = isReferenceEqual(mapper, identity2);
  let $1 = isReferenceEqual(child_mapper, identity2);
  if ($1) {
    return mapper;
  } else if ($) {
    return child_mapper;
  } else {
    return (msg) => {
      return mapper(child_mapper(msg));
    };
  }
}
function do_remove_children(loop$handlers, loop$path, loop$child_index, loop$children) {
  while (true) {
    let handlers = loop$handlers;
    let path = loop$path;
    let child_index = loop$child_index;
    let children = loop$children;
    if (children instanceof Empty) {
      return handlers;
    } else {
      let child = children.head;
      let rest = children.tail;
      let _pipe = handlers;
      let _pipe$1 = do_remove_child(_pipe, path, child_index, child);
      loop$handlers = _pipe$1;
      loop$path = path;
      loop$child_index = child_index + advance(child);
      loop$children = rest;
    }
  }
}
function do_remove_child(handlers, parent, child_index, child) {
  if (child instanceof Fragment) {
    let children = child.children;
    return do_remove_children(handlers, parent, child_index + 1, children);
  } else if (child instanceof Element) {
    let attributes = child.attributes;
    let children = child.children;
    let path = add2(parent, child_index, child.key);
    let _pipe = handlers;
    let _pipe$1 = remove_attributes(_pipe, path, attributes);
    return do_remove_children(_pipe$1, path, 0, children);
  } else if (child instanceof Text) {
    return handlers;
  } else {
    let attributes = child.attributes;
    let path = add2(parent, child_index, child.key);
    return remove_attributes(handlers, path, attributes);
  }
}
function remove_child(events, parent, child_index, child) {
  let handlers = do_remove_child(events.handlers, parent, child_index, child);
  let _record = events;
  return new Events(handlers, _record.dispatched_paths, _record.next_dispatched_paths);
}
function do_add_children(loop$handlers, loop$mapper, loop$path, loop$child_index, loop$children) {
  while (true) {
    let handlers = loop$handlers;
    let mapper = loop$mapper;
    let path = loop$path;
    let child_index = loop$child_index;
    let children = loop$children;
    if (children instanceof Empty) {
      return handlers;
    } else {
      let child = children.head;
      let rest = children.tail;
      let _pipe = handlers;
      let _pipe$1 = do_add_child(_pipe, mapper, path, child_index, child);
      loop$handlers = _pipe$1;
      loop$mapper = mapper;
      loop$path = path;
      loop$child_index = child_index + advance(child);
      loop$children = rest;
    }
  }
}
function do_add_child(handlers, mapper, parent, child_index, child) {
  if (child instanceof Fragment) {
    let children = child.children;
    let composed_mapper = compose_mapper(mapper, child.mapper);
    let child_index$1 = child_index + 1;
    return do_add_children(handlers, composed_mapper, parent, child_index$1, children);
  } else if (child instanceof Element) {
    let attributes = child.attributes;
    let children = child.children;
    let path = add2(parent, child_index, child.key);
    let composed_mapper = compose_mapper(mapper, child.mapper);
    let _pipe = handlers;
    let _pipe$1 = add_attributes(_pipe, composed_mapper, path, attributes);
    return do_add_children(_pipe$1, composed_mapper, path, 0, children);
  } else if (child instanceof Text) {
    return handlers;
  } else {
    let attributes = child.attributes;
    let path = add2(parent, child_index, child.key);
    let composed_mapper = compose_mapper(mapper, child.mapper);
    return add_attributes(handlers, composed_mapper, path, attributes);
  }
}
function add_child(events, mapper, parent, index3, child) {
  let handlers = do_add_child(events.handlers, mapper, parent, index3, child);
  let _record = events;
  return new Events(handlers, _record.dispatched_paths, _record.next_dispatched_paths);
}
function from_node(root3) {
  return add_child(new$3(), identity2, root2, 0, root3);
}
function add_children(events, mapper, path, child_index, children) {
  let handlers = do_add_children(events.handlers, mapper, path, child_index, children);
  let _record = events;
  return new Events(handlers, _record.dispatched_paths, _record.next_dispatched_paths);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element2(tag, attributes, children) {
  return element("", identity2, "", tag, attributes, children, empty2(), false, false);
}
function namespaced(namespace, tag, attributes, children) {
  return element("", identity2, namespace, tag, attributes, children, empty2(), false, false);
}
function text2(content) {
  return text("", identity2, content);
}
function none2() {
  return text("", identity2, "");
}
function count_fragment_children(loop$children, loop$count) {
  while (true) {
    let children = loop$children;
    let count = loop$count;
    if (children instanceof Empty) {
      return count;
    } else {
      let child = children.head;
      let rest = children.tail;
      loop$children = rest;
      loop$count = count + advance(child);
    }
  }
}
function fragment2(children) {
  return fragment("", identity2, children, empty2(), count_fragment_children(children, 0));
}

// build/dev/javascript/lustre/lustre/vdom/patch.mjs
class Patch extends CustomType {
  constructor(index3, removed, changes, children) {
    super();
    this.index = index3;
    this.removed = removed;
    this.changes = changes;
    this.children = children;
  }
}

class ReplaceText extends CustomType {
  constructor(kind, content) {
    super();
    this.kind = kind;
    this.content = content;
  }
}

class ReplaceInnerHtml extends CustomType {
  constructor(kind, inner_html) {
    super();
    this.kind = kind;
    this.inner_html = inner_html;
  }
}

class Update extends CustomType {
  constructor(kind, added, removed) {
    super();
    this.kind = kind;
    this.added = added;
    this.removed = removed;
  }
}

class Move extends CustomType {
  constructor(kind, key, before, count) {
    super();
    this.kind = kind;
    this.key = key;
    this.before = before;
    this.count = count;
  }
}

class RemoveKey extends CustomType {
  constructor(kind, key, count) {
    super();
    this.kind = kind;
    this.key = key;
    this.count = count;
  }
}

class Replace extends CustomType {
  constructor(kind, from, count, with$) {
    super();
    this.kind = kind;
    this.from = from;
    this.count = count;
    this.with = with$;
  }
}

class Insert extends CustomType {
  constructor(kind, children, before) {
    super();
    this.kind = kind;
    this.children = children;
    this.before = before;
  }
}

class Remove extends CustomType {
  constructor(kind, from, count) {
    super();
    this.kind = kind;
    this.from = from;
    this.count = count;
  }
}
function new$5(index3, removed, changes, children) {
  return new Patch(index3, removed, changes, children);
}
var replace_text_kind = 0;
function replace_text(content) {
  return new ReplaceText(replace_text_kind, content);
}
var replace_inner_html_kind = 1;
function replace_inner_html(inner_html) {
  return new ReplaceInnerHtml(replace_inner_html_kind, inner_html);
}
var update_kind = 2;
function update(added, removed) {
  return new Update(update_kind, added, removed);
}
var move_kind = 3;
function move(key, before, count) {
  return new Move(move_kind, key, before, count);
}
var remove_key_kind = 4;
function remove_key(key, count) {
  return new RemoveKey(remove_key_kind, key, count);
}
var replace_kind = 5;
function replace2(from, count, with$) {
  return new Replace(replace_kind, from, count, with$);
}
var insert_kind = 6;
function insert4(children, before) {
  return new Insert(insert_kind, children, before);
}
var remove_kind = 7;
function remove2(from, count) {
  return new Remove(remove_kind, from, count);
}

// build/dev/javascript/lustre/lustre/runtime/transport.mjs
class Mount extends CustomType {
  constructor(kind, open_shadow_root, will_adopt_styles, observed_attributes, observed_properties, vdom) {
    super();
    this.kind = kind;
    this.open_shadow_root = open_shadow_root;
    this.will_adopt_styles = will_adopt_styles;
    this.observed_attributes = observed_attributes;
    this.observed_properties = observed_properties;
    this.vdom = vdom;
  }
}

class Reconcile extends CustomType {
  constructor(kind, patch) {
    super();
    this.kind = kind;
    this.patch = patch;
  }
}

class Emit extends CustomType {
  constructor(kind, name, data) {
    super();
    this.kind = kind;
    this.name = name;
    this.data = data;
  }
}
class AttributeChanged extends CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}
class EventFired extends CustomType {
  constructor(kind, path, name, event2) {
    super();
    this.kind = kind;
    this.path = path;
    this.name = name;
    this.event = event2;
  }
}

// build/dev/javascript/lustre/lustre/vdom/diff.mjs
class Diff extends CustomType {
  constructor(patch, events) {
    super();
    this.patch = patch;
    this.events = events;
  }
}

class AttributeChange extends CustomType {
  constructor(added, removed, events) {
    super();
    this.added = added;
    this.removed = removed;
    this.events = events;
  }
}
function is_controlled(events, namespace, tag, path) {
  if (tag === "input") {
    if (namespace === "") {
      return has_dispatched_events(events, path);
    } else {
      return false;
    }
  } else if (tag === "select") {
    if (namespace === "") {
      return has_dispatched_events(events, path);
    } else {
      return false;
    }
  } else if (tag === "textarea") {
    if (namespace === "") {
      return has_dispatched_events(events, path);
    } else {
      return false;
    }
  } else {
    return false;
  }
}
function diff_attributes(loop$controlled, loop$path, loop$mapper, loop$events, loop$old, loop$new, loop$added, loop$removed) {
  while (true) {
    let controlled = loop$controlled;
    let path = loop$path;
    let mapper = loop$mapper;
    let events = loop$events;
    let old = loop$old;
    let new$6 = loop$new;
    let added = loop$added;
    let removed = loop$removed;
    if (new$6 instanceof Empty) {
      if (old instanceof Empty) {
        return new AttributeChange(added, removed, events);
      } else {
        let $ = old.head;
        if ($ instanceof Event2) {
          let prev = $;
          let old$1 = old.tail;
          let name = $.name;
          let removed$1 = prepend(prev, removed);
          let events$1 = remove_event(events, path, name);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events$1;
          loop$old = old$1;
          loop$new = new$6;
          loop$added = added;
          loop$removed = removed$1;
        } else {
          let prev = $;
          let old$1 = old.tail;
          let removed$1 = prepend(prev, removed);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events;
          loop$old = old$1;
          loop$new = new$6;
          loop$added = added;
          loop$removed = removed$1;
        }
      }
    } else if (old instanceof Empty) {
      let $ = new$6.head;
      if ($ instanceof Event2) {
        let next = $;
        let new$1 = new$6.tail;
        let name = $.name;
        let handler = $.handler;
        let added$1 = prepend(next, added);
        let events$1 = add_event(events, mapper, path, name, handler);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = old;
        loop$new = new$1;
        loop$added = added$1;
        loop$removed = removed;
      } else {
        let next = $;
        let new$1 = new$6.tail;
        let added$1 = prepend(next, added);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = old;
        loop$new = new$1;
        loop$added = added$1;
        loop$removed = removed;
      }
    } else {
      let next = new$6.head;
      let remaining_new = new$6.tail;
      let prev = old.head;
      let remaining_old = old.tail;
      let $ = compare3(prev, next);
      if ($ instanceof Lt) {
        if (prev instanceof Event2) {
          let name = prev.name;
          let removed$1 = prepend(prev, removed);
          let events$1 = remove_event(events, path, name);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events$1;
          loop$old = remaining_old;
          loop$new = new$6;
          loop$added = added;
          loop$removed = removed$1;
        } else {
          let removed$1 = prepend(prev, removed);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events;
          loop$old = remaining_old;
          loop$new = new$6;
          loop$added = added;
          loop$removed = removed$1;
        }
      } else if ($ instanceof Eq) {
        if (next instanceof Attribute) {
          if (prev instanceof Attribute) {
            let _block;
            let $1 = next.name;
            if ($1 === "value") {
              _block = controlled || prev.value !== next.value;
            } else if ($1 === "checked") {
              _block = controlled || prev.value !== next.value;
            } else if ($1 === "selected") {
              _block = controlled || prev.value !== next.value;
            } else {
              _block = prev.value !== next.value;
            }
            let has_changes = _block;
            let _block$1;
            if (has_changes) {
              _block$1 = prepend(next, added);
            } else {
              _block$1 = added;
            }
            let added$1 = _block$1;
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed;
          } else if (prev instanceof Event2) {
            let name = prev.name;
            let added$1 = prepend(next, added);
            let removed$1 = prepend(prev, removed);
            let events$1 = remove_event(events, path, name);
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events$1;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed$1;
          } else {
            let added$1 = prepend(next, added);
            let removed$1 = prepend(prev, removed);
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed$1;
          }
        } else if (next instanceof Property) {
          if (prev instanceof Property) {
            let _block;
            let $1 = next.name;
            if ($1 === "scrollLeft") {
              _block = true;
            } else if ($1 === "scrollRight") {
              _block = true;
            } else if ($1 === "value") {
              _block = controlled || !isEqual2(prev.value, next.value);
            } else if ($1 === "checked") {
              _block = controlled || !isEqual2(prev.value, next.value);
            } else if ($1 === "selected") {
              _block = controlled || !isEqual2(prev.value, next.value);
            } else {
              _block = !isEqual2(prev.value, next.value);
            }
            let has_changes = _block;
            let _block$1;
            if (has_changes) {
              _block$1 = prepend(next, added);
            } else {
              _block$1 = added;
            }
            let added$1 = _block$1;
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed;
          } else if (prev instanceof Event2) {
            let name = prev.name;
            let added$1 = prepend(next, added);
            let removed$1 = prepend(prev, removed);
            let events$1 = remove_event(events, path, name);
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events$1;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed$1;
          } else {
            let added$1 = prepend(next, added);
            let removed$1 = prepend(prev, removed);
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed$1;
          }
        } else if (prev instanceof Event2) {
          let name = next.name;
          let handler = next.handler;
          let has_changes = !isEqual(prev.prevent_default, next.prevent_default) || !isEqual(prev.stop_propagation, next.stop_propagation) || prev.immediate !== next.immediate || prev.debounce !== next.debounce || prev.throttle !== next.throttle;
          let _block;
          if (has_changes) {
            _block = prepend(next, added);
          } else {
            _block = added;
          }
          let added$1 = _block;
          let events$1 = add_event(events, mapper, path, name, handler);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events$1;
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = added$1;
          loop$removed = removed;
        } else {
          let name = next.name;
          let handler = next.handler;
          let added$1 = prepend(next, added);
          let removed$1 = prepend(prev, removed);
          let events$1 = add_event(events, mapper, path, name, handler);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events$1;
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = added$1;
          loop$removed = removed$1;
        }
      } else if (next instanceof Event2) {
        let name = next.name;
        let handler = next.handler;
        let added$1 = prepend(next, added);
        let events$1 = add_event(events, mapper, path, name, handler);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      } else {
        let added$1 = prepend(next, added);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      }
    }
  }
}
function do_diff(loop$old, loop$old_keyed, loop$new, loop$new_keyed, loop$moved, loop$moved_offset, loop$removed, loop$node_index, loop$patch_index, loop$path, loop$changes, loop$children, loop$mapper, loop$events) {
  while (true) {
    let old = loop$old;
    let old_keyed = loop$old_keyed;
    let new$6 = loop$new;
    let new_keyed = loop$new_keyed;
    let moved = loop$moved;
    let moved_offset = loop$moved_offset;
    let removed = loop$removed;
    let node_index = loop$node_index;
    let patch_index = loop$patch_index;
    let path = loop$path;
    let changes = loop$changes;
    let children = loop$children;
    let mapper = loop$mapper;
    let events = loop$events;
    if (new$6 instanceof Empty) {
      if (old instanceof Empty) {
        return new Diff(new Patch(patch_index, removed, changes, children), events);
      } else {
        let prev = old.head;
        let old$1 = old.tail;
        let _block;
        let $ = prev.key === "" || !contains(moved, prev.key);
        if ($) {
          _block = removed + advance(prev);
        } else {
          _block = removed;
        }
        let removed$1 = _block;
        let events$1 = remove_child(events, path, node_index, prev);
        loop$old = old$1;
        loop$old_keyed = old_keyed;
        loop$new = new$6;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset;
        loop$removed = removed$1;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$path = path;
        loop$changes = changes;
        loop$children = children;
        loop$mapper = mapper;
        loop$events = events$1;
      }
    } else if (old instanceof Empty) {
      let events$1 = add_children(events, mapper, path, node_index, new$6);
      let insert5 = insert4(new$6, node_index - moved_offset);
      let changes$1 = prepend(insert5, changes);
      return new Diff(new Patch(patch_index, removed, changes$1, children), events$1);
    } else {
      let next = new$6.head;
      let prev = old.head;
      if (prev.key !== next.key) {
        let new_remaining = new$6.tail;
        let old_remaining = old.tail;
        let next_did_exist = get(old_keyed, next.key);
        let prev_does_exist = get(new_keyed, prev.key);
        let prev_has_moved = contains(moved, prev.key);
        if (next_did_exist instanceof Ok) {
          if (prev_does_exist instanceof Ok) {
            if (prev_has_moved) {
              loop$old = old_remaining;
              loop$old_keyed = old_keyed;
              loop$new = new$6;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset - advance(prev);
              loop$removed = removed;
              loop$node_index = node_index;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes;
              loop$children = children;
              loop$mapper = mapper;
              loop$events = events;
            } else {
              let match = next_did_exist[0];
              let count = advance(next);
              let before = node_index - moved_offset;
              let move2 = move(next.key, before, count);
              let changes$1 = prepend(move2, changes);
              let moved$1 = insert2(moved, next.key);
              let moved_offset$1 = moved_offset + count;
              loop$old = prepend(match, old);
              loop$old_keyed = old_keyed;
              loop$new = new$6;
              loop$new_keyed = new_keyed;
              loop$moved = moved$1;
              loop$moved_offset = moved_offset$1;
              loop$removed = removed;
              loop$node_index = node_index;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes$1;
              loop$children = children;
              loop$mapper = mapper;
              loop$events = events;
            }
          } else {
            let count = advance(prev);
            let moved_offset$1 = moved_offset - count;
            let events$1 = remove_child(events, path, node_index, prev);
            let remove3 = remove_key(prev.key, count);
            let changes$1 = prepend(remove3, changes);
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new$6;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset$1;
            loop$removed = removed;
            loop$node_index = node_index;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = changes$1;
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        } else if (prev_does_exist instanceof Ok) {
          let before = node_index - moved_offset;
          let count = advance(next);
          let events$1 = add_child(events, mapper, path, node_index, next);
          let insert5 = insert4(toList([next]), before);
          let changes$1 = prepend(insert5, changes);
          loop$old = old;
          loop$old_keyed = old_keyed;
          loop$new = new_remaining;
          loop$new_keyed = new_keyed;
          loop$moved = moved;
          loop$moved_offset = moved_offset + count;
          loop$removed = removed;
          loop$node_index = node_index + count;
          loop$patch_index = patch_index;
          loop$path = path;
          loop$changes = changes$1;
          loop$children = children;
          loop$mapper = mapper;
          loop$events = events$1;
        } else {
          let prev_count = advance(prev);
          let next_count = advance(next);
          let change = replace2(node_index - moved_offset, prev_count, next);
          let _block;
          let _pipe = events;
          let _pipe$1 = remove_child(_pipe, path, node_index, prev);
          _block = add_child(_pipe$1, mapper, path, node_index, next);
          let events$1 = _block;
          loop$old = old_remaining;
          loop$old_keyed = old_keyed;
          loop$new = new_remaining;
          loop$new_keyed = new_keyed;
          loop$moved = moved;
          loop$moved_offset = moved_offset - prev_count + next_count;
          loop$removed = removed;
          loop$node_index = node_index + next_count;
          loop$patch_index = patch_index;
          loop$path = path;
          loop$changes = prepend(change, changes);
          loop$children = children;
          loop$mapper = mapper;
          loop$events = events$1;
        }
      } else {
        let $ = old.head;
        if ($ instanceof Fragment) {
          let $1 = new$6.head;
          if ($1 instanceof Fragment) {
            let next$1 = $1;
            let new$1 = new$6.tail;
            let prev$1 = $;
            let old$1 = old.tail;
            let node_index$1 = node_index + 1;
            let prev_count = prev$1.children_count;
            let next_count = next$1.children_count;
            let composed_mapper = compose_mapper(mapper, next$1.mapper);
            let child = do_diff(prev$1.children, prev$1.keyed_children, next$1.children, next$1.keyed_children, empty_set(), moved_offset, 0, node_index$1, -1, path, empty_list, children, composed_mapper, events);
            let _block;
            let $2 = child.patch.removed > 0;
            if ($2) {
              let remove_from = node_index$1 + next_count - moved_offset;
              let patch = remove2(remove_from, child.patch.removed);
              _block = append(child.patch.changes, prepend(patch, changes));
            } else {
              _block = append(child.patch.changes, changes);
            }
            let changes$1 = _block;
            loop$old = old$1;
            loop$old_keyed = old_keyed;
            loop$new = new$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset + next_count - prev_count;
            loop$removed = removed;
            loop$node_index = node_index$1 + next_count;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = changes$1;
            loop$children = child.patch.children;
            loop$mapper = mapper;
            loop$events = child.events;
          } else {
            let next$1 = $1;
            let new_remaining = new$6.tail;
            let prev$1 = $;
            let old_remaining = old.tail;
            let prev_count = advance(prev$1);
            let next_count = advance(next$1);
            let change = replace2(node_index - moved_offset, prev_count, next$1);
            let _block;
            let _pipe = events;
            let _pipe$1 = remove_child(_pipe, path, node_index, prev$1);
            _block = add_child(_pipe$1, mapper, path, node_index, next$1);
            let events$1 = _block;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset - prev_count + next_count;
            loop$removed = removed;
            loop$node_index = node_index + next_count;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        } else if ($ instanceof Element) {
          let $1 = new$6.head;
          if ($1 instanceof Element) {
            let next$1 = $1;
            let prev$1 = $;
            if (prev$1.namespace === next$1.namespace && prev$1.tag === next$1.tag) {
              let new$1 = new$6.tail;
              let old$1 = old.tail;
              let composed_mapper = compose_mapper(mapper, next$1.mapper);
              let child_path = add2(path, node_index, next$1.key);
              let controlled = is_controlled(events, next$1.namespace, next$1.tag, child_path);
              let $2 = diff_attributes(controlled, child_path, composed_mapper, events, prev$1.attributes, next$1.attributes, empty_list, empty_list);
              let added_attrs = $2.added;
              let removed_attrs = $2.removed;
              let events$1 = $2.events;
              let _block;
              if (removed_attrs instanceof Empty) {
                if (added_attrs instanceof Empty) {
                  _block = empty_list;
                } else {
                  _block = toList([update(added_attrs, removed_attrs)]);
                }
              } else {
                _block = toList([update(added_attrs, removed_attrs)]);
              }
              let initial_child_changes = _block;
              let child = do_diff(prev$1.children, prev$1.keyed_children, next$1.children, next$1.keyed_children, empty_set(), 0, 0, 0, node_index, child_path, initial_child_changes, empty_list, composed_mapper, events$1);
              let _block$1;
              let $3 = child.patch;
              let $4 = $3.children;
              if ($4 instanceof Empty) {
                let $5 = $3.changes;
                if ($5 instanceof Empty) {
                  let $6 = $3.removed;
                  if ($6 === 0) {
                    _block$1 = children;
                  } else {
                    _block$1 = prepend(child.patch, children);
                  }
                } else {
                  _block$1 = prepend(child.patch, children);
                }
              } else {
                _block$1 = prepend(child.patch, children);
              }
              let children$1 = _block$1;
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes;
              loop$children = children$1;
              loop$mapper = mapper;
              loop$events = child.events;
            } else {
              let next$2 = $1;
              let new_remaining = new$6.tail;
              let prev$2 = $;
              let old_remaining = old.tail;
              let prev_count = advance(prev$2);
              let next_count = advance(next$2);
              let change = replace2(node_index - moved_offset, prev_count, next$2);
              let _block;
              let _pipe = events;
              let _pipe$1 = remove_child(_pipe, path, node_index, prev$2);
              _block = add_child(_pipe$1, mapper, path, node_index, next$2);
              let events$1 = _block;
              loop$old = old_remaining;
              loop$old_keyed = old_keyed;
              loop$new = new_remaining;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset - prev_count + next_count;
              loop$removed = removed;
              loop$node_index = node_index + next_count;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = prepend(change, changes);
              loop$children = children;
              loop$mapper = mapper;
              loop$events = events$1;
            }
          } else {
            let next$1 = $1;
            let new_remaining = new$6.tail;
            let prev$1 = $;
            let old_remaining = old.tail;
            let prev_count = advance(prev$1);
            let next_count = advance(next$1);
            let change = replace2(node_index - moved_offset, prev_count, next$1);
            let _block;
            let _pipe = events;
            let _pipe$1 = remove_child(_pipe, path, node_index, prev$1);
            _block = add_child(_pipe$1, mapper, path, node_index, next$1);
            let events$1 = _block;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset - prev_count + next_count;
            loop$removed = removed;
            loop$node_index = node_index + next_count;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        } else if ($ instanceof Text) {
          let $1 = new$6.head;
          if ($1 instanceof Text) {
            let next$1 = $1;
            let prev$1 = $;
            if (prev$1.content === next$1.content) {
              let new$1 = new$6.tail;
              let old$1 = old.tail;
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes;
              loop$children = children;
              loop$mapper = mapper;
              loop$events = events;
            } else {
              let next$2 = $1;
              let new$1 = new$6.tail;
              let old$1 = old.tail;
              let child = new$5(node_index, 0, toList([replace_text(next$2.content)]), empty_list);
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes;
              loop$children = prepend(child, children);
              loop$mapper = mapper;
              loop$events = events;
            }
          } else {
            let next$1 = $1;
            let new_remaining = new$6.tail;
            let prev$1 = $;
            let old_remaining = old.tail;
            let prev_count = advance(prev$1);
            let next_count = advance(next$1);
            let change = replace2(node_index - moved_offset, prev_count, next$1);
            let _block;
            let _pipe = events;
            let _pipe$1 = remove_child(_pipe, path, node_index, prev$1);
            _block = add_child(_pipe$1, mapper, path, node_index, next$1);
            let events$1 = _block;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset - prev_count + next_count;
            loop$removed = removed;
            loop$node_index = node_index + next_count;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        } else {
          let $1 = new$6.head;
          if ($1 instanceof UnsafeInnerHtml) {
            let next$1 = $1;
            let new$1 = new$6.tail;
            let prev$1 = $;
            let old$1 = old.tail;
            let composed_mapper = compose_mapper(mapper, next$1.mapper);
            let child_path = add2(path, node_index, next$1.key);
            let $2 = diff_attributes(false, child_path, composed_mapper, events, prev$1.attributes, next$1.attributes, empty_list, empty_list);
            let added_attrs = $2.added;
            let removed_attrs = $2.removed;
            let events$1 = $2.events;
            let _block;
            if (removed_attrs instanceof Empty) {
              if (added_attrs instanceof Empty) {
                _block = empty_list;
              } else {
                _block = toList([update(added_attrs, removed_attrs)]);
              }
            } else {
              _block = toList([update(added_attrs, removed_attrs)]);
            }
            let child_changes = _block;
            let _block$1;
            let $3 = prev$1.inner_html === next$1.inner_html;
            if ($3) {
              _block$1 = child_changes;
            } else {
              _block$1 = prepend(replace_inner_html(next$1.inner_html), child_changes);
            }
            let child_changes$1 = _block$1;
            let _block$2;
            if (child_changes$1 instanceof Empty) {
              _block$2 = children;
            } else {
              _block$2 = prepend(new$5(node_index, 0, child_changes$1, toList([])), children);
            }
            let children$1 = _block$2;
            loop$old = old$1;
            loop$old_keyed = old_keyed;
            loop$new = new$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = changes;
            loop$children = children$1;
            loop$mapper = mapper;
            loop$events = events$1;
          } else {
            let next$1 = $1;
            let new_remaining = new$6.tail;
            let prev$1 = $;
            let old_remaining = old.tail;
            let prev_count = advance(prev$1);
            let next_count = advance(next$1);
            let change = replace2(node_index - moved_offset, prev_count, next$1);
            let _block;
            let _pipe = events;
            let _pipe$1 = remove_child(_pipe, path, node_index, prev$1);
            _block = add_child(_pipe$1, mapper, path, node_index, next$1);
            let events$1 = _block;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset - prev_count + next_count;
            loop$removed = removed;
            loop$node_index = node_index + next_count;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        }
      }
    }
  }
}
function diff(events, old, new$6) {
  return do_diff(toList([old]), empty2(), toList([new$6]), empty2(), empty_set(), 0, 0, 0, 0, root2, empty_list, empty_list, identity2, tick(events));
}

// build/dev/javascript/lustre/lustre/vdom/reconciler.ffi.mjs
class Reconciler {
  offset = 0;
  #root = null;
  #dispatch = () => {};
  #useServerEvents = false;
  #exposeKeys = false;
  constructor(root3, dispatch, { useServerEvents = false, exposeKeys = false } = {}) {
    this.#root = root3;
    this.#dispatch = dispatch;
    this.#useServerEvents = useServerEvents;
    this.#exposeKeys = exposeKeys;
  }
  mount(vdom) {
    appendChild(this.#root, this.#createChild(this.#root, 0, vdom));
  }
  #stack = [];
  push(patch) {
    const offset = this.offset;
    if (offset) {
      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
          case move_kind:
            change.before = (change.before | 0) + offset;
            break;
          case remove_kind:
          case replace_kind:
            change.from = (change.from | 0) + offset;
            break;
        }
      });
      iterate(patch.children, (child) => {
        child.index = (child.index | 0) + offset;
      });
    }
    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }
  #reconcile() {
    const self = this;
    while (self.#stack.length) {
      const { node, patch } = self.#stack.pop();
      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
            self.#insert(node, change.children, change.before);
            break;
          case move_kind:
            self.#move(node, change.key, change.before, change.count);
            break;
          case remove_key_kind:
            self.#removeKey(node, change.key, change.count);
            break;
          case remove_kind:
            self.#remove(node, change.from, change.count);
            break;
          case replace_kind:
            self.#replace(node, change.from, change.count, change.with);
            break;
          case replace_text_kind:
            self.#replaceText(node, change.content);
            break;
          case replace_inner_html_kind:
            self.#replaceInnerHtml(node, change.inner_html);
            break;
          case update_kind:
            self.#update(node, change.added, change.removed);
            break;
        }
      });
      if (patch.removed) {
        self.#remove(node, node.childNodes.length - patch.removed, patch.removed);
      }
      let lastIndex = -1;
      let lastChild = null;
      iterate(patch.children, (child) => {
        const index3 = child.index | 0;
        const next = lastChild && lastIndex - index3 === 1 ? lastChild.previousSibling : childAt(node, index3);
        self.#stack.push({ node: next, patch: child });
        lastChild = next;
        lastIndex = index3;
      });
    }
  }
  #insert(node, children, before) {
    const fragment3 = createDocumentFragment();
    let childIndex = before | 0;
    iterate(children, (child) => {
      const el = this.#createChild(node, childIndex, child);
      appendChild(fragment3, el);
      childIndex += advance(child);
    });
    insertBefore(node, fragment3, childAt(node, before));
  }
  #move(node, key, before, count) {
    let el = getKeyedChild(node, key);
    const beforeEl = childAt(node, before);
    for (let i = 0;i < count && el !== null; ++i) {
      const next = el.nextSibling;
      if (SUPPORTS_MOVE_BEFORE) {
        node.moveBefore(el, beforeEl);
      } else {
        insertBefore(node, el, beforeEl);
      }
      el = next;
    }
  }
  #removeKey(node, key, count) {
    this.#removeFromChild(node, getKeyedChild(node, key), count);
  }
  #remove(node, from, count) {
    this.#removeFromChild(node, childAt(node, from), count);
  }
  #removeFromChild(parent, child, count) {
    while (count-- > 0 && child !== null) {
      const next = child.nextSibling;
      const key = child[meta].key;
      if (key) {
        parent[meta].keyedChildren.delete(key);
      }
      for (const [_, { timeout }] of child[meta].debouncers ?? []) {
        clearTimeout(timeout);
      }
      parent.removeChild(child);
      child = next;
    }
  }
  #replace(parent, from, count, child) {
    this.#remove(parent, from, count);
    const el = this.#createChild(parent, from, child);
    insertBefore(parent, el, childAt(parent, from));
  }
  #replaceText(node, content) {
    node.data = content ?? "";
  }
  #replaceInnerHtml(node, inner_html) {
    node.innerHTML = inner_html ?? "";
  }
  #update(node, added, removed) {
    iterate(removed, (attribute3) => {
      const name = attribute3.name;
      if (node[meta].handlers.has(name)) {
        node.removeEventListener(name, handleEvent);
        node[meta].handlers.delete(name);
        if (node[meta].throttles.has(name)) {
          node[meta].throttles.delete(name);
        }
        if (node[meta].debouncers.has(name)) {
          clearTimeout(node[meta].debouncers.get(name).timeout);
          node[meta].debouncers.delete(name);
        }
      } else {
        node.removeAttribute(name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });
    iterate(added, (attribute3) => {
      this.#createAttribute(node, attribute3);
    });
  }
  #createChild(parent, index3, vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = createChildElement(parent, index3, vnode);
        this.#createAttributes(node, vnode);
        this.#insert(node, vnode.children);
        return node;
      }
      case text_kind: {
        return createChildText(parent, index3, vnode);
      }
      case fragment_kind: {
        const node = createDocumentFragment();
        const head = createChildText(parent, index3, vnode);
        appendChild(node, head);
        let childIndex = index3 + 1;
        iterate(vnode.children, (child) => {
          appendChild(node, this.#createChild(parent, childIndex, child));
          childIndex += advance(child);
        });
        return node;
      }
      case unsafe_inner_html_kind: {
        const node = createChildElement(parent, index3, vnode);
        this.#createAttributes(node, vnode);
        this.#replaceInnerHtml(node, vnode.inner_html);
        return node;
      }
    }
  }
  #createAttributes(node, { key, attributes }) {
    if (this.#exposeKeys && key) {
      node.setAttribute("data-lustre-key", key);
    }
    iterate(attributes, (attribute3) => this.#createAttribute(node, attribute3));
  }
  #createAttribute(node, attribute3) {
    const { debouncers, handlers, throttles } = node[meta];
    const {
      kind,
      name,
      value,
      prevent_default: prevent,
      stop_propagation: stop,
      immediate: immediate2,
      include,
      debounce: debounceDelay,
      throttle: throttleDelay
    } = attribute3;
    switch (kind) {
      case attribute_kind: {
        const valueOrDefault = value ?? "";
        if (name === "virtual:defaultValue") {
          node.defaultValue = valueOrDefault;
          return;
        }
        if (valueOrDefault !== node.getAttribute(name)) {
          node.setAttribute(name, valueOrDefault);
        }
        SYNCED_ATTRIBUTES[name]?.added?.(node, value);
        break;
      }
      case property_kind:
        node[name] = value;
        break;
      case event_kind: {
        if (handlers.has(name)) {
          node.removeEventListener(name, handleEvent);
        }
        node.addEventListener(name, handleEvent, {
          passive: prevent.kind === never_kind
        });
        if (throttleDelay > 0) {
          const throttle = throttles.get(name) ?? {};
          throttle.delay = throttleDelay;
          throttles.set(name, throttle);
        } else {
          throttles.delete(name);
        }
        if (debounceDelay > 0) {
          const debounce = debouncers.get(name) ?? {};
          debounce.delay = debounceDelay;
          debouncers.set(name, debounce);
        } else {
          clearTimeout(debouncers.get(name)?.timeout);
          debouncers.delete(name);
        }
        handlers.set(name, (event2) => {
          if (prevent.kind === always_kind)
            event2.preventDefault();
          if (stop.kind === always_kind)
            event2.stopPropagation();
          const type = event2.type;
          const path = event2.currentTarget[meta].path;
          const data = this.#useServerEvents ? createServerEvent(event2, include ?? []) : event2;
          const throttle = throttles.get(type);
          if (throttle) {
            const now = Date.now();
            const last = throttle.last || 0;
            if (now > last + throttle.delay) {
              throttle.last = now;
              throttle.lastEvent = event2;
              this.#dispatch(data, path, type, immediate2);
            }
          }
          const debounce = debouncers.get(type);
          if (debounce) {
            clearTimeout(debounce.timeout);
            debounce.timeout = setTimeout(() => {
              if (event2 === throttles.get(type)?.lastEvent)
                return;
              this.#dispatch(data, path, type, immediate2);
            }, debounce.delay);
          }
          if (!throttle && !debounce) {
            this.#dispatch(data, path, type, immediate2);
          }
        });
        break;
      }
    }
  }
}
var iterate = (list4, callback) => {
  if (Array.isArray(list4)) {
    for (let i = 0;i < list4.length; i++) {
      callback(list4[i]);
    }
  } else if (list4) {
    for (list4;list4.tail; list4 = list4.tail) {
      callback(list4.head);
    }
  }
};
var appendChild = (node, child) => node.appendChild(child);
var insertBefore = (parent, node, referenceNode) => parent.insertBefore(node, referenceNode ?? null);
var createChildElement = (parent, index3, { key, tag, namespace }) => {
  const node = document().createElementNS(namespace || NAMESPACE_HTML, tag);
  initialiseMetadata(parent, node, index3, key);
  return node;
};
var createChildText = (parent, index3, { key, content }) => {
  const node = document().createTextNode(content ?? "");
  initialiseMetadata(parent, node, index3, key);
  return node;
};
var createDocumentFragment = () => document().createDocumentFragment();
var childAt = (node, at) => node.childNodes[at | 0];
var meta = Symbol("lustre");
var initialiseMetadata = (parent, node, index3 = 0, key = "") => {
  const segment = `${key || index3}`;
  switch (node.nodeType) {
    case ELEMENT_NODE:
    case DOCUMENT_FRAGMENT_NODE:
      node[meta] = {
        key,
        path: segment,
        keyedChildren: new Map,
        handlers: new Map,
        throttles: new Map,
        debouncers: new Map
      };
      break;
    case TEXT_NODE:
      node[meta] = { key };
      break;
  }
  if (parent && parent[meta] && key) {
    parent[meta].keyedChildren.set(key, new WeakRef(node));
  }
  if (parent && parent[meta] && parent[meta].path) {
    node[meta].path = `${parent[meta].path}${separator_element}${segment}`;
  }
};
var getKeyedChild = (node, key) => node[meta].keyedChildren.get(key).deref();
var handleEvent = (event2) => {
  const target = event2.currentTarget;
  const handler = target[meta].handlers.get(event2.type);
  if (event2.type === "submit") {
    event2.detail ??= {};
    event2.detail.formData = [...new FormData(event2.target).entries()];
  }
  handler(event2);
};
var createServerEvent = (event2, include = []) => {
  const data = {};
  if (event2.type === "input" || event2.type === "change") {
    include.push("target.value");
  }
  if (event2.type === "submit") {
    include.push("detail.formData");
  }
  for (const property3 of include) {
    const path = property3.split(".");
    for (let i = 0, input = event2, output = data;i < path.length; i++) {
      if (i === path.length - 1) {
        output[path[i]] = input[path[i]];
        break;
      }
      output = output[path[i]] ??= {};
      input = input[path[i]];
    }
  }
  return data;
};
var syncedBooleanAttribute = (name) => {
  return {
    added(node) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    }
  };
};
var syncedAttribute = (name) => {
  return {
    added(node, value) {
      node[name] = value;
    }
  };
};
var SYNCED_ATTRIBUTES = {
  checked: syncedBooleanAttribute("checked"),
  selected: syncedBooleanAttribute("selected"),
  value: syncedAttribute("value"),
  autofocus: {
    added(node) {
      queueMicrotask(() => node.focus?.());
    }
  },
  autoplay: {
    added(node) {
      try {
        node.play?.();
      } catch (e) {
        console.error(e);
      }
    }
  }
};

// build/dev/javascript/lustre/lustre/vdom/virtualise.ffi.mjs
var virtualise = (root3) => {
  const vdom = virtualiseNode(null, root3, "");
  if (vdom === null || vdom.children instanceof Empty) {
    const empty3 = emptyTextNode(root3);
    root3.appendChild(empty3);
    return none2();
  } else if (vdom.children instanceof NonEmpty && vdom.children.tail instanceof Empty) {
    return vdom.children.head;
  } else {
    const head = emptyTextNode(root3);
    root3.insertBefore(head, root3.firstChild);
    return fragment2(vdom.children);
  }
};
var emptyTextNode = (parent) => {
  const node = document().createTextNode("");
  initialiseMetadata(parent, node);
  return node;
};
var virtualiseNode = (parent, node, index3) => {
  switch (node.nodeType) {
    case ELEMENT_NODE: {
      const key = node.getAttribute("data-lustre-key");
      initialiseMetadata(parent, node, index3, key);
      if (key) {
        node.removeAttribute("data-lustre-key");
      }
      const tag = node.localName;
      const namespace = node.namespaceURI;
      const isHtmlElement = !namespace || namespace === NAMESPACE_HTML;
      if (isHtmlElement && INPUT_ELEMENTS.includes(tag)) {
        virtualiseInputEvents(tag, node);
      }
      const attributes = virtualiseAttributes(node);
      const children = virtualiseChildNodes(node);
      const vnode = isHtmlElement ? element2(tag, attributes, children) : namespaced(namespace, tag, attributes, children);
      return key ? to_keyed(key, vnode) : vnode;
    }
    case TEXT_NODE:
      initialiseMetadata(parent, node, index3);
      return node.data ? text2(node.data) : null;
    case DOCUMENT_FRAGMENT_NODE:
      initialiseMetadata(parent, node, index3);
      return node.childNodes.length > 0 ? fragment2(virtualiseChildNodes(node)) : null;
    default:
      return null;
  }
};
var INPUT_ELEMENTS = ["input", "select", "textarea"];
var virtualiseInputEvents = (tag, node) => {
  const value = node.value;
  const checked = node.checked;
  if (tag === "input" && node.type === "checkbox" && !checked)
    return;
  if (tag === "input" && node.type === "radio" && !checked)
    return;
  if (node.type !== "checkbox" && node.type !== "radio" && !value)
    return;
  queueMicrotask(() => {
    node.value = value;
    node.checked = checked;
    node.dispatchEvent(new Event("input", { bubbles: true }));
    node.dispatchEvent(new Event("change", { bubbles: true }));
    if (document().activeElement !== node) {
      node.dispatchEvent(new Event("blur", { bubbles: true }));
    }
  });
};
var virtualiseChildNodes = (node) => {
  let children = null;
  let index3 = 0;
  let child = node.firstChild;
  let ptr = null;
  while (child) {
    const vnode = virtualiseNode(node, child, index3);
    const next = child.nextSibling;
    if (vnode) {
      const list_node = new NonEmpty(vnode, null);
      if (ptr) {
        ptr = ptr.tail = list_node;
      } else {
        ptr = children = list_node;
      }
      index3 += 1;
    } else {
      node.removeChild(child);
    }
    child = next;
  }
  if (!ptr)
    return empty_list;
  ptr.tail = empty_list;
  return children;
};
var virtualiseAttributes = (node) => {
  let index3 = node.attributes.length;
  let attributes = empty_list;
  while (index3-- > 0) {
    attributes = new NonEmpty(virtualiseAttribute(node.attributes[index3]), attributes);
  }
  return attributes;
};
var virtualiseAttribute = (attr) => {
  const name = attr.localName;
  const value = attr.value;
  return attribute2(name, value);
};

// build/dev/javascript/lustre/lustre/runtime/client/runtime.ffi.mjs
var is_browser = () => !!document();
class Runtime {
  constructor(root3, [model, effects], view, update2) {
    this.root = root3;
    this.#model = model;
    this.#view = view;
    this.#update = update2;
    this.#reconciler = new Reconciler(this.root, (event2, path, name) => {
      const [events, result] = handle(this.#events, path, name, event2);
      this.#events = events;
      if (result.isOk()) {
        const handler = result[0];
        if (handler.stop_propagation)
          event2.stopPropagation();
        if (handler.prevent_default)
          event2.preventDefault();
        this.dispatch(handler.message, false);
      }
    });
    this.#vdom = virtualise(this.root);
    this.#events = new$3();
    this.#shouldFlush = true;
    this.#tick(effects);
  }
  root = null;
  set offset(offset) {
    this.#reconciler.offset = offset;
  }
  dispatch(msg, immediate2 = false) {
    this.#shouldFlush ||= immediate2;
    if (this.#shouldQueue) {
      this.#queue.push(msg);
    } else {
      const [model, effects] = this.#update(this.#model, msg);
      this.#model = model;
      this.#tick(effects);
    }
  }
  emit(event2, data) {
    const target = this.root.host ?? this.root;
    target.dispatchEvent(new CustomEvent(event2, {
      detail: data,
      bubbles: true,
      composed: true
    }));
  }
  #model;
  #view;
  #update;
  #vdom;
  #events;
  #reconciler;
  #shouldQueue = false;
  #queue = [];
  #beforePaint = empty_list;
  #afterPaint = empty_list;
  #renderTimer = null;
  #shouldFlush = false;
  #actions = {
    dispatch: (msg, immediate2) => this.dispatch(msg, immediate2),
    emit: (event2, data) => this.emit(event2, data),
    select: () => {},
    root: () => this.root
  };
  #tick(effects) {
    this.#shouldQueue = true;
    while (true) {
      for (let list4 = effects.synchronous;list4.tail; list4 = list4.tail) {
        list4.head(this.#actions);
      }
      this.#beforePaint = listAppend(this.#beforePaint, effects.before_paint);
      this.#afterPaint = listAppend(this.#afterPaint, effects.after_paint);
      if (!this.#queue.length)
        break;
      [this.#model, effects] = this.#update(this.#model, this.#queue.shift());
    }
    this.#shouldQueue = false;
    if (this.#shouldFlush) {
      cancelAnimationFrame(this.#renderTimer);
      this.#render();
    } else if (!this.#renderTimer) {
      this.#renderTimer = requestAnimationFrame(() => {
        this.#render();
      });
    }
  }
  #render() {
    this.#shouldFlush = false;
    this.#renderTimer = null;
    const next = this.#view(this.#model);
    const { patch, events } = diff(this.#events, this.#vdom, next);
    this.#events = events;
    this.#vdom = next;
    this.#reconciler.push(patch);
    if (this.#beforePaint instanceof NonEmpty) {
      const effects = makeEffect(this.#beforePaint);
      this.#beforePaint = empty_list;
      queueMicrotask(() => {
        this.#shouldFlush = true;
        this.#tick(effects);
      });
    }
    if (this.#afterPaint instanceof NonEmpty) {
      const effects = makeEffect(this.#afterPaint);
      this.#afterPaint = empty_list;
      requestAnimationFrame(() => {
        this.#shouldFlush = true;
        this.#tick(effects);
      });
    }
  }
}
function makeEffect(synchronous) {
  return {
    synchronous,
    after_paint: empty_list,
    before_paint: empty_list
  };
}
function listAppend(a, b) {
  if (a instanceof Empty) {
    return b;
  } else if (b instanceof Empty) {
    return a;
  } else {
    return append(a, b);
  }
}
var copiedStyleSheets = new WeakMap;

// build/dev/javascript/lustre/lustre/runtime/server/runtime.mjs
class ClientDispatchedMessage extends CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}
class ClientRegisteredCallback extends CustomType {
  constructor(callback) {
    super();
    this.callback = callback;
  }
}

class ClientDeregisteredCallback extends CustomType {
  constructor(callback) {
    super();
    this.callback = callback;
  }
}
class EffectDispatchedMessage extends CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

class EffectEmitEvent extends CustomType {
  constructor(name, data) {
    super();
    this.name = name;
    this.data = data;
  }
}
class SelfDispatchedMessages extends CustomType {
  constructor(messages, effect) {
    super();
    this.messages = messages;
    this.effect = effect;
  }
}

class SystemRequestedShutdown extends CustomType {
}

// build/dev/javascript/lustre/lustre/component.mjs
class Config2 extends CustomType {
  constructor(open_shadow_root, adopt_styles, attributes, properties, is_form_associated, on_form_autofill, on_form_reset, on_form_restore) {
    super();
    this.open_shadow_root = open_shadow_root;
    this.adopt_styles = adopt_styles;
    this.attributes = attributes;
    this.properties = properties;
    this.is_form_associated = is_form_associated;
    this.on_form_autofill = on_form_autofill;
    this.on_form_reset = on_form_reset;
    this.on_form_restore = on_form_restore;
  }
}
function new$6(options) {
  let init = new Config2(false, true, empty_dict(), empty_dict(), false, option_none, option_none, option_none);
  return fold(options, init, (config, option) => {
    return option.apply(config);
  });
}

// build/dev/javascript/lustre/lustre/runtime/client/spa.ffi.mjs
class Spa {
  static start({ init, update: update2, view }, selector, flags) {
    if (!is_browser())
      return new Error(new NotABrowser);
    const root3 = selector instanceof HTMLElement ? selector : document().querySelector(selector);
    if (!root3)
      return new Error(new ElementNotFound(selector));
    return new Ok(new Spa(root3, init(flags), update2, view));
  }
  #runtime;
  constructor(root3, [init, effects], update2, view) {
    this.#runtime = new Runtime(root3, [init, effects], view, update2);
  }
  send(message) {
    switch (message.constructor) {
      case EffectDispatchedMessage: {
        this.dispatch(message.message, false);
        break;
      }
      case EffectEmitEvent: {
        this.emit(message.name, message.data);
        break;
      }
      case SystemRequestedShutdown:
        break;
    }
  }
  dispatch(msg, immediate2) {
    this.#runtime.dispatch(msg, immediate2);
  }
  emit(event2, data) {
    this.#runtime.emit(event2, data);
  }
}
var start = Spa.start;

// build/dev/javascript/lustre/lustre/runtime/server/runtime.ffi.mjs
class Runtime2 {
  #model;
  #update;
  #view;
  #on_attribute_change;
  #vdom;
  #events;
  #callbacks = new Set;
  constructor([model, effects], view, update2, on_attribute_change) {
    this.#model = model;
    this.#update = update2;
    this.#view = view;
    this.#on_attribute_change = on_attribute_change;
    this.#vdom = this.#view(this.#model);
    this.#events = from_node(this.#vdom);
    this.#tick(effects.all, false);
  }
  send(message) {
    if (this.#model === null)
      return;
    switch (message.constructor) {
      case ClientDispatchedMessage:
        switch (message.message.constructor) {
          case AttributeChanged: {
            const { name, value } = message.messgae;
            let effects = [];
            const decoder = this.#on_attribute_change.get(name);
            if (!decoder)
              break;
            const result = run(value, decoder);
            if (result.constructor !== Ok)
              break;
            const [model, more_effects] = this.#update(this.#model, result[0]);
            this.#model = model;
            effects.push(more_effects);
            while (effects.length) {
              this.#tick(effects.shift().all);
            }
          }
          case EventFired: {
            const { path, name, event: event2 } = message.message;
            const [events, result] = handle(this.#events, path, name, event2);
            this.#events = events;
            if (result.constructor === Ok) {
              this.dispatch(result[0].message);
            }
            return;
          }
        }
      case ClientRegisteredCallback: {
        if (this.#callbacks.has(message.callback))
          return;
        const mount = new Mount(this.#vdom);
        this.#callbacks.add(message.callback);
        message.callback(mount);
        return;
      }
      case ClientDeregisteredCallback: {
        this.#callbacks.delete(message.callback);
        return;
      }
      case EffectDispatchedMessage: {
        this.dispatch(message.message);
        return;
      }
      case EffectEmitEvent: {
        const event2 = new Emit(message.name, message.data);
        for (const callback of this.#callbacks) {
          callback(event2);
        }
        return;
      }
      case SelfDispatchedMessages: {
        let messages = message.messages;
        let effects = [message.effect];
        for (let list4 = messages;messages.tail; list4 = list4.tail) {
          const [model, more_effects] = this.#update(this.#model, list4.head);
          this.#model = model;
          effects.push(more_effects);
        }
        while (effects.length) {
          this.#tick(effects.shift().all);
        }
        return;
      }
      case SystemRequestedShutdown: {
        this.#model = null;
        this.#update = null;
        this.#view = null;
        this.#on_attribute_change = null;
        this.#events = new$3();
        this.#callbacks.clear();
      }
    }
  }
  dispatch(msg) {
    const [model, effects] = this.#update(this.#model, msg);
    this.#model = model;
    this.#tick(effects.all, immediate);
  }
  #tick(effects) {
    const queue = [];
    const effect_params = {
      root: null,
      emit: (event2, data) => this.#emit(event2, data),
      dispatch: (msg) => queue.push(msg),
      select: () => {}
    };
    while (true) {
      for (let list4 = effects;list4.tail; list4 = list4.tail) {
        list4.head(effect_params);
      }
      if (!queue.length) {
        break;
      }
      const msg = queue.shift();
      [this.#model, effects] = this.#update(this.#model, msg);
    }
    this.#render();
  }
  #render() {
    const next = this.#view(this.#model);
    const { patch, events } = diff(this.#events, this.#vdom, next);
    this.#events = events;
    this.#vdom = next;
    const reconcile = new Reconcile(patch);
    for (const callback of this.#callbacks) {
      callback(reconcile);
    }
  }
  #emit(event2, data) {
    const message = new Emit(event2, data);
    for (const callback of this.#callbacks) {
      callback(message);
    }
  }
}

// build/dev/javascript/lustre/lustre.mjs
class App extends CustomType {
  constructor(init, update2, view, config) {
    super();
    this.init = init;
    this.update = update2;
    this.view = view;
    this.config = config;
  }
}
class ElementNotFound extends CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
}

class NotABrowser extends CustomType {
}
function application(init, update2, view) {
  return new App(init, update2, view, new$6(empty_list));
}
function element3(view) {
  return application((_) => {
    return [undefined, none()];
  }, (_, _1) => {
    return [undefined, none()];
  }, (_) => {
    return view;
  });
}
function start3(app, selector, start_args) {
  return guard(!is_browser(), new Error(new NotABrowser), () => {
    return start(app, selector, start_args);
  });
}
// build/dev/javascript/app/app.mjs
var FILEPATH = "src/app.gleam";
function main() {
  let $ = start3(element3(text2("Hello, world")), "#app", undefined);
  if (!($ instanceof Ok)) {
    throw makeError("let_assert", FILEPATH, "app", 9, "main", "Pattern match failed, no pattern matched the value.", { value: $, start: 219, end: 313, pattern_start: 230, pattern_end: 235 });
  }
  return $;
}

// .lustre/build/app.mjs
main();
