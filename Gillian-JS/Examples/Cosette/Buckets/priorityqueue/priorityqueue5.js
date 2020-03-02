'use strict';

/**
 * Top level namespace for Buckets,
 * a JavaScript data structure library.
 * @id buckets
 */
var buckets = {};

/**
 * Default function to compare element order.
 * @function
 * @private
 */
/* @id base_defaultCompare */
buckets.defaultCompare = function (a, b) {
    if (a < b) {
        return -1;
    }
    if (a === b) {
        return 0;
    }
    return 1;
};

/**
 * Default function to test equality.
 * @function
 * @private
 */
/* @id base_defaultEquals */
buckets.defaultEquals = function (a, b) {
    return a === b;
};

/**
 * Default function to convert an object to a string.
 * @function
 * @private
 */
/* @id base_defaultToString */
buckets.defaultToString = function (item) {
    if (item === null) {
        return 'BUCKETS_NULL';
    }
    if (buckets.isUndefined(item)) {
        return 'BUCKETS_UNDEFINED';
    }
    if (buckets.isString(item)) {
        return item;
    }
    return item.toString();
};

/**
 * Checks if the given argument is a function.
 * @function
 * @private
 */
/* @id base_isFunction */
buckets.isFunction = function (func) {
    return (typeof func) === 'function';
};

/**
 * Checks if the given argument is undefined.
 * @function
 * @private
 */
/* @id base_isUndefined */
buckets.isUndefined = function (obj) {
    return obj === undefined;
};

/**
 * Checks if the given argument is a string.
 * @function
 * @private
 */
/* @id base_isString */
buckets.isString = function (obj) {
    return Object.prototype.toString.call(obj) === '[object String]';
};

/**
 * Reverses a compare function.
 * @function
 * @private
 */
/* @id base_reverseCompareFunction */
buckets.reverseCompareFunction = function (compareFunction) {
    if (!buckets.isFunction(compareFunction)) {
        /* @id base_reverseCompareFunction_inner1 */
        return function (a, b) {
            if (a < b) {
                return 1;
            }
            if (a === b) {
                return 0;
            }
            return -1;
        };
    }
    /* @id base_reverseCompareFunction_inner2 */
    return function (d, v) {
        return compareFunction(d, v) * -1;
    };

};

/**
 * Returns an equal function given a compare function.
 * @function
 * @private
 */
/* @id base_compareToEquals */
buckets.compareToEquals = function (compareFunction) {
    /* @id base_compareToEquals_inner */
    return function (a, b) {
        return compareFunction(a, b) === 0;
    };
};

// ------------------------------- arrays.js ---------------------------------

/**
 * @namespace Contains various functions for manipulating arrays.
 */
buckets.arrays = {};

/**
 * Returns the index of the first occurrence of the specified item
 * within the specified array.
 * @param {*} array The array.
 * @param {*} item The element to search for.
 * @param {function(Object,Object):boolean=} equalsFunction Optional function to
 * check equality between two elements. Receives two arguments and returns true if they are equal.
 * @return {number} The index of the first occurrence of the specified element
 * or -1 if not found.
 */
buckets.arrays.indexOf = function (array, item, equalsFunction) {
    var equals = equalsFunction || buckets.defaultEquals,
        length = array.length,
        i;
    for (i = 0; i < length; i += 1) {
        if (equals(array[i], item)) {
            return i;
        }
    }
    return -1;
};

/**
 * Returns the index of the last occurrence of the specified element
 * within the specified array.
 * @param {*} array The array.
 * @param {Object} item The element to search for.
 * @param {function(Object,Object):boolean=} equalsFunction Optional function to
 * check equality between two elements. Receives two arguments and returns true if they are equal.
 * @return {number} The index of the last occurrence of the specified element
 * within the specified array or -1 if not found.
 */
buckets.arrays.lastIndexOf = function (array, item, equalsFunction) {
    var equals = equalsFunction || buckets.defaultEquals,
        length = array.length,
        i;
    for (i = length - 1; i >= 0; i -= 1) {
        if (equals(array[i], item)) {
            return i;
        }
    }
    return -1;
};

/**
 * Returns true if the array contains the specified element.
 * @param {*} array The array.
 * @param {Object} item The element to search for.
 * @param {function(Object,Object):boolean=} equalsFunction Optional function to
 * check equality between two elements. Receives two arguments and returns true if they are equal.
 * @return {boolean} True if the specified array contains the specified element.
 */
buckets.arrays.contains = function (array, item, equalsFunction) {
    return buckets.arrays.indexOf(array, item, equalsFunction) >= 0;
};

/**
 * Removes the first ocurrence of the specified element from the specified array.
 * @param {*} array The array.
 * @param {*} item The element to remove.
 * @param {function(Object,Object):boolean=} equalsFunction Optional function to
 * check equality between two elements. Receives two arguments and returns true if they are equal.
 * @return {boolean} True If the array changed after this call.
 */
buckets.arrays.remove = function (array, item, equalsFunction) {
    var index = buckets.arrays.indexOf(array, item, equalsFunction);
    if (index < 0) {
        return false;
    }
    array.splice(index, 1);
    return true;
};

/**
 * Returns the number of elements in the array equal
 * to the specified element.
 * @param {Array} array The array.
 * @param {Object} item The element.
 * @param {function(Object,Object):boolean=} equalsFunction Optional function to
 * check equality between two elements. Receives two arguments and returns true if they are equal.
 * @return {number} The number of elements in the specified array.
 * equal to the specified item.
 */
buckets.arrays.frequency = function (array, item, equalsFunction) {
    var equals = equalsFunction || buckets.defaultEquals,
        length = array.length,
        freq = 0,
        i;
    for (i = 0; i < length; i += 1) {
        if (equals(array[i], item)) {
            freq += 1;
        }
    }
    return freq;
};

/**
 * Returns true if the provided arrays are equal.
 * Two arrays are considered equal if both contain the same number
 * of elements and all corresponding pairs of elements
 * are equal and are in the same order.
 * @param {Array} array1
 * @param {Array} array2
 * @param {function(Object,Object):boolean=} equalsFunction Optional function to
 * check equality between two elements. Receives two arguments and returns true if they are equal.
 * @return {boolean} True if the two arrays are equal.
 */
buckets.arrays.equals = function (array1, array2, equalsFunction) {
    var equals = equalsFunction || buckets.defaultEquals,
        length = array1.length,
        i;

    if (array1.length !== array2.length) {
        return false;
    }
    for (i = 0; i < length; i += 1) {
        if (!equals(array1[i], array2[i])) {
            return false;
        }
    }
    return true;
};

/**
 * Returns a shallow copy of the specified array.
 * @param {*} array The array to copy.
 * @return {Array} A copy of the specified array.
 */
buckets.arrays.copy = function (array) {
    return array.concat();
};

/**
 * Swaps the elements at the specified positions in the specified array.
 * @param {Array} array The array.
 * @param {number} i The index of the first element.
 * @param {number} j The index of second element.
 * @return {boolean} True if the array is defined and the indexes are valid.
 */
buckets.arrays.swap = function (array, i, j) {
    var temp;

    if (i < 0 || i >= array.length || j < 0 || j >= array.length) {
        return false;
    }
    temp = array[i];
    array[i] = array[j];
    array[j] = temp;
    return true;
};

/**
 * Executes the provided function once per element present in the array.
 * @param {Array} array The array.
 * @param {function(Object):*} callback Function to execute,
 * invoked with an element as argument. To break the iteration you can
 * optionally return false in the callback.
 */
buckets.arrays.forEach = function (array, callback) {
    var lenght = array.length,
        i;
    for (i = 0; i < lenght; i += 1) {
        if (callback(array[i]) === false) {
            return;
        }
    }
};

// -------------------------------- heap.js ----------------------------------

/**
 * Creates an empty binary heap.
 * @class
 * <p>A heap is a binary tree that maintains the heap property:
 * Every node is less than or equal to each of its children. 
 * This implementation uses an array as the underlying storage.</p>
 * <p>If the inserted elements are custom objects, a compare function must be provided 
 * at construction time, otherwise the <=, === and >= operators are
 * used to compare elements.</p>
 * <p>Example:</p>
 * <pre>
 * function compare(a, b) {
 *  if (a is less than b by some ordering criterion) {
 *     return -1;
 *  } if (a is greater than b by the ordering criterion) {
 *     return 1;
 *  }
 *  // a must be equal to b
 *  return 0;
 * }
 * </pre>
 *
 * <p>To create a Max-Heap (greater elements on top) you can a provide a
 * reverse compare function.</p>
 * <p>Example:</p>
 *
 * <pre>
 * function reverseCompare(a, b) {
 *  if (a is less than b by some ordering criterion) {
 *     return 1;
 *  } if (a is greater than b by the ordering criterion) {
 *     return -1;
 *  }
 *  // a must be equal to b
 *  return 0;
 * }
 * </pre>
 *
 * @constructor
 * @param {function(Object,Object):number=} compareFunction Optional
 * function used to compare two elements. Must return a negative integer,
 * zero, or a positive integer as the first argument is less than, equal to,
 * or greater than the second.
 */
buckets.Heap = function (compareFunction) {

    /** 
     * @exports heap as buckets.Heap
     * @private
     */
    var heap = {},
        // Array used to store the elements of the heap.
        data = [],
        // Function used to compare elements.
        compare = compareFunction || buckets.defaultCompare;

    // Moves the node at the given index up to its proper place in the heap.
    function siftUp(index) {
        var parent;
        // Returns the index of the parent of the node at the given index.
        function parentIndex(nodeIndex) {
            return Math.floor((nodeIndex - 1) / 2);
        }

        parent = parentIndex(index);
        while (index > 0 && compare(data[parent], data[index]) > 0) {
            buckets.arrays.swap(data, parent, index);
            index = parent;
            parent = parentIndex(index);
        }
    }

    // Moves the node at the given index down to its proper place in the heap.
    function siftDown(nodeIndex) {
        var min;
        // Returns the index of the left child of the node at the given index.
        function leftChildIndex(nodeIndex) {
            return (2 * nodeIndex) + 1;
        }

        // Returns the index of the right child of the node at the given index.
        function rightChildIndex(nodeIndex) {
            return (2 * nodeIndex) + 2;
        }

        // Returns the index of the smaller child node if it exists, -1 otherwise.
        function minIndex(leftChild, rightChild) {
            if (rightChild >= data.length) {
                if (leftChild >= data.length) {
                    return -1;
                }
                return leftChild;
            }
            if (compare(data[leftChild], data[rightChild]) <= 0) {
                return leftChild;
            }
            return rightChild;
        }

        // Minimum child index
        min = minIndex(leftChildIndex(nodeIndex), rightChildIndex(nodeIndex));

        while (min >= 0 && compare(data[nodeIndex], data[min]) > 0) {
            buckets.arrays.swap(data, min, nodeIndex);
            nodeIndex = min;
            min = minIndex(leftChildIndex(nodeIndex), rightChildIndex(nodeIndex));
        }
    }

    /**
     * Retrieves but does not remove the root (minimum) element of the heap.
     * @return {*} The value at the root of the heap. Returns undefined if the
     * heap is empty.
     */
    heap.peek = function () {
        if (data.length > 0) {
            return data[0];
        }
        return undefined;
    };

    /**
     * Adds the given element into the heap.
     * @param {*} element The element.
     * @return True if the element was added or false if it is undefined.
     */
    heap.add = function (element) {
        if (buckets.isUndefined(element)) {
            return undefined;
        }
        data.push(element);
        siftUp(data.length - 1);
        return true;
    };

    /**
     * Retrieves and removes the root (minimum) element of the heap.
     * @return {*} The removed element or
     * undefined if the heap is empty.
     */
    heap.removeRoot = function () {
        var obj;
        if (data.length > 0) {
            obj = data[0];
            data[0] = data[data.length - 1];
            data.splice(data.length - 1, 1);
            if (data.length > 0) {
                siftDown(0);
            }
            return obj;
        }
        return undefined;
    };

    /**
     * Returns true if the heap contains the specified element.
     * @param {Object} element Element to search for.
     * @return {boolean} True if the Heap contains the specified element, false
     * otherwise.
     */
    heap.contains = function (element) {
        var equF = buckets.compareToEquals(compare);
        return buckets.arrays.contains(data, element, equF);
    };

    /**
     * Returns the number of elements in the heap.
     * @return {number} The number of elements in the heap.
     */
    heap.size = function () {
        return data.length;
    };

    /**
     * Checks if the heap is empty.
     * @return {boolean} True if the heap contains no elements; false
     * otherwise.
     */
    heap.isEmpty = function () {
        return data.length <= 0;
    };

    /**
     * Removes all the elements from the heap.
     */
    heap.clear = function () {
        data.length = 0;
    };

    /**
     * Executes the provided function once per element present in the heap in
     * no particular order.
     * @param {function(Object):*} callback Function to execute,
     * invoked with an element as argument. To break the iteration you can
     * optionally return false.
     */
    heap.forEach = function (callback) {
        buckets.arrays.forEach(data, callback);
    };

    /**
     * Returns an array containing all the elements in the heap in no
     * particular order.
     * @return {Array.<*>} An array containing all the elements in the heap
     * in no particular order.
     */
    heap.toArray = function () {
        return buckets.arrays.copy(data);
    };

    /**
     * Returns true if the binary heap is equal to another heap.
     * Two heaps are equal if they have the same elements.
     * @param {buckets.Heap} other The other heap.
     * @return {boolean} True if the heap is equal to the given heap.
     */
    heap.equals = function (other) {
        var thisArray, otherArray, eqF;

        if (buckets.isUndefined(other) || typeof other.removeRoot !== 'function') {
            return false;
        }
        if (heap.size() !== other.size()) {
            return false;
        }

        thisArray = heap.toArray();
        otherArray = other.toArray();
        eqF = buckets.compareToEquals(compare);
        thisArray.sort(compare);
        otherArray.sort(compare);

        return buckets.arrays.equals(thisArray, otherArray, eqF);
    };

    return heap;
};

// ---------------------------- priorityqueue.js -----------------------------

/**
 * Creates an empty priority queue.
 * @class <p>In a priority queue each element is associated with a "priority",
 * elements are dequeued in highest-priority-first order (the elements with the
 * highest priority are dequeued first). This implementation uses a binary 
 * heap as the underlying storage.</p>
 *
 * <p>If the inserted elements are custom objects, a compare function must be provided,
 * otherwise the <=, === and >= operators are used to compare object priority.</p>
 * <p>Example:</p>
 * <pre>
 * function compare(a, b) {
 *  if (a is less than b by some ordering criterion) {
 *     return -1;
 *  } if (a is greater than b by the ordering criterion) {
 *     return 1;
 *  }
 *  // a must be equal to b
 *  return 0;
 * }
 * </pre>
 * @constructor
 * @param {function(Object,Object):number=} compareFunction Optional
 * function used to compare two element priorities. Must return a negative integer,
 * zero, or a positive integer as the first argument is less than, equal to,
 * or greater than the second.
 */
buckets.PriorityQueue = function (compareFunction) {

    /** 
     * @exports pQueue as buckets.PriorityQueue
     * @private
     */
    var pQueue = {},
        // Reversed compare function
        compare = buckets.reverseCompareFunction(compareFunction),
        // Underlying storage
        heap = new buckets.Heap(compare);

    /**
     * Inserts the specified element into the priority queue.
     * @param {Object} element The element to insert.
     * @return {boolean} True if the element was inserted, or false if it's undefined.
     */
    /* @id priorityqueue_enqueue */
    pQueue.enqueue = function (element) {
        return heap.add(element);
    };

    /**
     * Inserts the specified element into the priority queue. It's equivalent to enqueue.
     * @param {Object} element The element to insert.
     * @return {boolean} True if the element was inserted, or false if it's undefined.
     */
    /* @id priorityqueue_add */
    pQueue.add = function (element) {
        return heap.add(element);
    };

    /**
     * Retrieves and removes the highest priority element of the queue.
     * @return {*} The highest priority element of the queue,
     * or undefined if the queue is empty.
     */
    /* @id priorityqueue_dequeue */
    pQueue.dequeue = function () {
        var elem;
        if (heap.size() !== 0) {
            elem = heap.peek();
            heap.removeRoot();
            return elem;
        }
        return undefined;
    };

    /**
     * Retrieves, but does not remove, the highest priority element of the queue.
     * @return {*} The highest priority element of the queue, or undefined if the queue is empty.
     */
    /* @id priorityqueue_peek */
    pQueue.peek = function () {
        return heap.peek();
    };

    /**
     * Returns true if the priority queue contains the specified element.
     * @param {Object} element Element to search for.
     * @return {boolean} True if the priority queue contains the specified element,
     * false otherwise.
     */
    /* @id priorityqueue_contains */
    pQueue.contains = function (element) {
        return heap.contains(element);
    };

    /**
     * Checks if the priority queue is empty.
     * @return {boolean} True if and only if the priority queue contains no items, false
     * otherwise.
     */
    /* @id priorityqueue_isEmpty */
    pQueue.isEmpty = function () {
        return heap.isEmpty();
    };

    /**
     * Returns the number of elements in the priority queue.
     * @return {number} The number of elements in the priority queue.
     */
    /* @id priorityqueue_size */
    pQueue.size = function () {
        return heap.size();
    };

    /**
     * Removes all elements from the priority queue.
     */
    /* @id priorityqueue_clear */
    pQueue.clear = function () {
        heap.clear();
    };

    /**
     * Executes the provided function once per element present in the queue in
     * no particular order.
     * @param {function(Object):*} callback Function to execute, it's
     * invoked one element as argument. To break the iteration you can
     * optionally return false inside the callback.
     */
    /* @id priorityqueue_forEach */
    pQueue.forEach = function (callback) {
        heap.forEach(callback);
    };

    /**
     * Returns an array containing all the elements in the queue in no
     * particular order.
     * @return {Array.<*>} An array containing all the elements in the queue
     * in no particular order.
     */
    /* @id priorityqueue_toArray */
    pQueue.toArray = function () {
        return heap.toArray();
    };

    /**
     * Returns true if the queue is equal to another queue.
     * Two priority queues are equal if they have the same elements.
     * @param {buckets.PriorityQueue} other The other queue.
     * @return {boolean} True if the queue is equal to the given queue.
     */
    /* @id priorityqueue_equals */
    pQueue.equals = function (other) {
        var thisArray, otherArray, eqF;

        if (buckets.isUndefined(other) || typeof other.dequeue !== 'function') {
            return false;
        }
        if (pQueue.size() !== other.size()) {
            return false;
        }

        thisArray = pQueue.toArray();
        otherArray = other.toArray();
        eqF = buckets.compareToEquals(compare);
        thisArray.sort(compare);
        otherArray.sort(compare);

        return buckets.arrays.equals(thisArray, otherArray, eqF);
    };

    return pQueue;
};

// ------------------------------- our tests ----------------------------------

var pqueue = new buckets.PriorityQueue();

var x1 = symb_number(x1);
var x2 = symb_number(x2);
var x3 = symb_number(x3);

pqueue.enqueue(x1);
pqueue.enqueue(x2);
pqueue.enqueue(x3);

pqueue.dequeue();

var pqueue2 = new buckets.PriorityQueue();

var res1 = pqueue.equals(pqueue2);
Assert(not res1);

pqueue.forEach(function (x) {
  pqueue2.add(x);
});
var res2 = pqueue2.equals(pqueue);
Assert(res2);

var ar = pqueue.toArray();
var res3 = pqueue.equals(ar);
Assert(not res3);