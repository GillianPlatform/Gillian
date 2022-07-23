// --------------------------------- _base.js --------------------------------

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

// ---------------------------- linkedlist.js --------------------------------

/**
 * Creates an empty Linked List.
 * @class A linked list is a sequence of items arranged one after 
 * another. The size is not fixed and it can grow or shrink 
 * on demand. One of the main benefits of a linked list is that 
 * you can add or remove elements at both ends in constant time. 
 * One disadvantage of a linked list against an array is 
 * that it doesn’t provide constant time random access.
 * @constructor
 */
buckets.LinkedList = function () {

    /** 
     * @exports list as buckets.LinkedList
     * @private
     */
    var list = {},
        // Number of elements in the list
        nElements = 0,
        // First node in the list
        firstNode,
        // Last node in the list
        lastNode;

    // Returns the node at the specified index.
    function nodeAtIndex(index) {
        var node, i;
        if (index < 0 || index >= nElements) {
            return undefined;
        }
        if (index === (nElements - 1)) {
            return lastNode;
        }
        node = firstNode;
        for (i = 0; i < index; i += 1) {
            node = node.next;
        }
        return node;
    }

    /**
     * Adds an element to the list.
     * @param {Object} item Element to be added.
     * @param {number=} index Optional index to add the element. If no index is specified
     * the element is added to the end of the list.
     * @return {boolean} True if the element was added or false if the index is invalid
     * or if the element is undefined.
     */
    list.add = function (item, index) {
        var newNode, prev;

        if (buckets.isUndefined(index)) {
            index = nElements;
        }
        if (index < 0 || index > nElements || buckets.isUndefined(item)) {
            return false;
        }
        newNode = {
            element: item,
            next: undefined
        };
        if (nElements === 0) {
            // First node in the list.
            firstNode = newNode;
            lastNode = newNode;
        } else if (index === nElements) {
            // Insert at the end.
            lastNode.next = newNode;
            lastNode = newNode;
        } else if (index === 0) {
            // Change first node.
            newNode.next = firstNode;
            firstNode = newNode;
        } else {
            prev = nodeAtIndex(index - 1);
            newNode.next = prev.next;
            prev.next = newNode;
        }
        nElements += 1;
        return true;
    };

    /**
     * Returns the first element in the list.
     * @return {*} The first element in the list or undefined if the list is
     * empty.
     */
    list.first = function () {
        if (firstNode !== undefined) {
            return firstNode.element;
        }
        return undefined;
    };

    /**
     * Returns the last element in the list.
     * @return {*} The last element in the list or undefined if the list is
     * empty.
     */
    list.last = function () {
        if (lastNode !== undefined) {
            return lastNode.element;
        }
        return undefined;
    };

    /**
     * Returns the element at the specified position in the list.
     * @param {number} index Desired index.
     * @return {*} The element at the given index or undefined if the index is
     * out of bounds.
     */
    list.elementAtIndex = function (index) {
        var node = nodeAtIndex(index);
        if (node === undefined) {
            return undefined;
        }
        return node.element;
    };


    /**
     * Returns the index of the first occurrence of the
     * specified element, or -1 if the list does not contain the element.
     * <p>If the elements inside the list are
     * not comparable with the === operator, a custom equals function should be
     * provided to perform searches, that function must receive two arguments and
     * return true if they are equal, false otherwise. Example:</p>
     *
     * <pre>
     * var petsAreEqualByName = function(pet1, pet2) {
     *  return pet1.name === pet2.name;
     * }
     * </pre>
     * @param {Object} item Element to search for.
     * @param {function(Object,Object):boolean=} equalsFunction Optional
     * function used to check if two elements are equal.
     * @return {number} The index in the list of the first occurrence
     * of the specified element, or -1 if the list does not contain the
     * element.
     */
    list.indexOf = function (item, equalsFunction) {
        var equalsF = equalsFunction || buckets.defaultEquals,
            currentNode = firstNode,
            index = 0;
        if (buckets.isUndefined(item)) {
            return -1;
        }

        while (currentNode !== undefined) {
            if (equalsF(currentNode.element, item)) {
                return index;
            }
            index += 1;
            currentNode = currentNode.next;
        }
        return -1;
    };

    /**
     * Returns true if the list contains the specified element.
     * <p>If the elements inside the list are
     * not comparable with the === operator, a custom equals function should be
     * provided to perform searches, that function must receive two arguments and
     * return true if they are equal, false otherwise. Example:</p>
     *
     * <pre>
     * var petsAreEqualByName = function(pet1, pet2) {
     *  return pet1.name === pet2.name;
     * }
     * </pre>
     * @param {Object} item Element to search for.
     * @param {function(Object,Object):boolean=} equalsFunction Optional
     * function used to check if two elements are equal.
     * @return {boolean} True if the list contains the specified element, false
     * otherwise.
     */
    list.contains = function (item, equalsFunction) {
        return (list.indexOf(item, equalsFunction) >= 0);
    };

    /**
     * Removes the first occurrence of the specified element in the list.
     * <p>If the elements inside the list are
     * not comparable with the === operator, a custom equals function should be
     * provided to perform searches, that function must receive two arguments and
     * return true if they are equal, false otherwise. Example:</p>
     * <pre>
     * var petsAreEqualByName = function(pet1, pet2) {
     *  return pet1.name === pet2.name;
     * }
     * </pre>
     * @param {Object} item Element to be removed from the list, if present.
     * @return {boolean} True if the list contained the specified element.
     */
    list.remove = function (item, equalsFunction) {
        var equalsF = equalsFunction || buckets.defaultEquals,
            currentNode = firstNode,
            previous;

        if (nElements < 1 || buckets.isUndefined(item)) {
            return false;
        }

        while (currentNode !== undefined) {

            if (equalsF(currentNode.element, item)) {

                if (currentNode === firstNode) {
                    firstNode = firstNode.next;
                    if (currentNode === lastNode) {
                        lastNode = undefined;
                    }
                } else if (currentNode === lastNode) {
                    lastNode = previous;
                    previous.next = currentNode.next;
                    currentNode.next = undefined;
                } else {
                    previous.next = currentNode.next;
                    currentNode.next = undefined;
                }
                nElements = nElements - 1;
                return true;
            }
            previous = currentNode;
            currentNode = currentNode.next;
        }
        return false;
    };

    /**
     * Removes all the elements from the list.
     */
    list.clear = function () {
        firstNode = undefined;
        lastNode = undefined;
        nElements = 0;
    };

    /**
     * Returns true if the list is equal to another list.
     * Two lists are equal if they have the same elements in the same order.
     * @param {buckets.LinkedList} other The other list.
     * @param {function(Object,Object):boolean=} equalsFunction Optional
     * function to check if two elements are equal. If the elements in the lists
     * are custom objects you should provide a custom equals function, otherwise
     * the === operator is used to check equality between elements.
     * @return {boolean} true if the list is equal to the given list.
     */
    list.equals = function (other, equalsFunction) {
        var eqf = equalsFunction || buckets.defaultEquals,
            isEqual = true,
            node = firstNode;

        if (buckets.isUndefined(other) || typeof other.elementAtIndex !== 'function') {
            return false;
        }
        if (list.size() !== other.size()) {
            return false;
        }

        other.forEach(function (element) {
            isEqual = eqf(element, node.element);
            node = node.next;
            return isEqual;
        });

        return isEqual;
    };

    /**
     * Removes the element at the specified position in the list.
     * @param {number} index Given index.
     * @return {*} Removed element or undefined if the index is out of bounds.
     */
    list.removeElementAtIndex = function (index) {
        var element, previous;

        if (index < 0 || index >= nElements) {
            return undefined;
        }

        if (nElements === 1) {
            //First node in the list.
            element = firstNode.element;
            firstNode = undefined;
            lastNode = undefined;
        } else {
            previous = nodeAtIndex(index - 1);
            if (previous === undefined) {
                element = firstNode.element;
                firstNode = firstNode.next;
            } else if (previous.next === lastNode) {
                element = lastNode.element;
                lastNode = previous;
            }
            if (previous !== undefined) {
                element = previous.next.element;
                previous.next = previous.next.next;
            }
        }
        nElements -= 1;
        return element;
    };

    /**
     * Executes the provided function once per element present in the list in order.
     * @param {function(Object):*} callback Function to execute, it is
     * invoked with one argument: the element value, to break the iteration you can
     * optionally return false inside the callback.
     */
    list.forEach = function (callback) {
        var currentNode = firstNode;
        while (currentNode !== undefined) {
            if (callback(currentNode.element) === false) {
                break;
            }
            currentNode = currentNode.next;
        }
    };

    /**
     * Reverses the order of the elements in the linked list (makes the last
     * element first, and the first element last).
     * @memberOf buckets.LinkedList
     */
    list.reverse = function () {
        var current = firstNode,
            previous,
            temp;
        while (current !== undefined) {
            temp = current.next;
            current.next = previous;
            previous = current;
            current = temp;
        }
        temp = firstNode;
        firstNode = lastNode;
        lastNode = temp;
    };


    /**
     * Returns an array containing all the elements in the list in proper
     * sequence.
     * @return {Array.<*>} An array containing all the elements in the list,
     * in proper sequence.
     */
    list.toArray = function () {
        var result = [];
        list.forEach(function (element) {
            result.push(element);
        });
        return result;
    };

    /**
     * Returns the number of elements in the list.
     * @return {number} The number of elements in the list.
     */
    list.size = function () {
        return nElements;
    };

    /**
     * Returns true if the list contains no elements.
     * @return {boolean} true if the list contains no elements.
     */
    list.isEmpty = function () {
        return nElements <= 0;
    };

    return list;
};

// ------------------------------- queue.js ----------------------------------

/**
 * Creates an empty queue.
 * @class A queue is a First-In-First-Out (FIFO) data structure, the first
 * element added to the queue will be the first one to be removed. This
 * implementation uses a linked list as the underlying storage.
 * @constructor
 */
buckets.Queue = function () {

    /** 
     * @exports queue as buckets.Queue
     * @private
     */
    var queue = {},
        // Underlying list containing the elements.
        list = new buckets.LinkedList();

    /**
     * Inserts the specified element into the end of the queue.
     * @param {Object} elem The element to insert.
     * @return {boolean} True if the element was inserted, or false if it's undefined.
     */
    queue.enqueue = function (elem) {
        return list.add(elem);
    };

    /**
     * Inserts the specified element into the end of the queue. Equivalent to enqueue.
     * @param {Object} elem The element to insert.
     * @return {boolean} True if the element was inserted, or false if it's undefined.
     */
    queue.add = function (elem) {
        return list.add(elem);
    };

    /**
     * Retrieves and removes the head of the queue.
     * @return {*} The head of the queue, or undefined if the queue is empty.
     */
    queue.dequeue = function () {
        var elem;
        if (list.size() !== 0) {
            elem = list.first();
            list.removeElementAtIndex(0);
            return elem;
        }
        return undefined;
    };

    /**
     * Retrieves, but does not remove, the head of the queue.
     * @return {*} The head of the queue, or undefined if the queue is empty.
     */
    queue.peek = function () {
        if (list.size() !== 0) {
            return list.first();
        }
        return undefined;
    };

    /**
     * Returns the number of elements in the queue.
     * @return {number} The number of elements in the queue.
     */
    queue.size = function () {
        return list.size();
    };

    /**
     * Returns true if the queue contains the specified element.
     * <p>If the elements inside the queue are
     * not comparable with the === operator, a custom equals function should be
     * provided to perform searches, the function must receive two arguments and
     * return true if they are equal, false otherwise. Example:</p>
     *
     * <pre>
     * var petsAreEqualByName = function(pet1, pet2) {
     *  return pet1.name === pet2.name;
     * }
     * </pre>
     * @param {Object} elem Element to search for.
     * @param {function(Object,Object):boolean=} equalsFunction Optional
     * function to check if two elements are equal.
     * @return {boolean} True if the queue contains the specified element,
     * false otherwise.
     */
    queue.contains = function (elem, equalsFunction) {
        return list.contains(elem, equalsFunction);
    };

    /**
     * Checks if the queue is empty.
     * @return {boolean} True if and only if the queue contains no items.
     */
    queue.isEmpty = function () {
        return list.size() <= 0;
    };

    /**
     * Removes all the elements from the queue.
     */
    queue.clear = function () {
        list.clear();
    };

    /**
     * Executes the provided function once per each element present in the queue in
     * FIFO order.
     * @param {function(Object):*} callback Function to execute, it's
     * invoked an element as argument, to break the iteration you can
     * optionally return false inside the callback.
     */
    queue.forEach = function (callback) {
        list.forEach(callback);
    };

    /**
     * Returns an array containing all the elements in the queue in FIFO
     * order.
     * @return {Array.<*>} An array containing all the elements in the queue
     * in FIFO order.
     */
    queue.toArray = function () {
        return list.toArray();
    };

    /**
     * Returns true if the queue is equal to another queue.
     * Two queues are equal if they have the same elements in the same order.
     * @param {buckets.Queue} other The other queue.
     * @param {function(Object,Object):boolean=} equalsFunction Optional
     * function to check if two elements are equal. If the elements in the queues
     * are custom objects you should provide a custom equals function, otherwise
     * the === operator is used to check equality between elements.
     * @return {boolean} True if the queue is equal to the given queue.
     */
    queue.equals = function (other, equalsFunction) {
        var eqf, isEqual, thisElement;
        if (buckets.isUndefined(other) || typeof other.dequeue !== 'function') {
            return false;
        }
        if (queue.size() !== other.size()) {
            return false;
        }
        eqf = equalsFunction || buckets.defaultEquals;
        isEqual = true;
        other.forEach(function (element) {
            thisElement = queue.dequeue();
            queue.enqueue(thisElement);
            isEqual = eqf(thisElement, element);
            return isEqual;
        });
        return isEqual;
    };

    return queue;
};


// ------------------------------- bstree.js----------------------------------


/**
 * Creates an empty binary search tree.
 * @class <p> Binary search trees keep their elements in sorted order, so that 
 * lookup and other operations can use the principle of binary search. In a BST
 * the element in any node is larger than the elements in the node's
 * left sub-tree and smaller than the elements in the node's right sub-tree.</p>
 * <p>If the inserted elements are custom objects, a compare function must
 * be provided at construction time, otherwise the <=, === and >= operators are
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
 * @constructor
 * @param {function(Object,Object):number=} compareFunction Optional
 * function used to compare two elements. Must return a negative integer,
 * zero, or a positive integer as the first argument is less than, equal to,
 * or greater than the second.
 */
buckets.BSTree = function (compareFunction) {

    /** 
     * @exports tree as buckets.BSTree
     * @private
     */
    var tree = {},
        // Function to compare elements.
        compare = compareFunction || buckets.defaultCompare,
        // Number of elements in the tree.
        nElements = 0,
        // The root node of the tree.
        root;

    // Returns the sub-node containing the specified element or undefined.
    /* @id bstree_searchNode */
    function searchNode(root, element) {
        var node = root,
            cmp;
        while (node !== undefined && cmp !== 0) {
            cmp = compare(element, node.element);
            if (cmp < 0) {
                node = node.leftCh;
            } else if (cmp > 0) {
                node = node.rightCh;
            }
        }
        return node;
    }

    // Returns the sub-node containing the minimum element or undefined.
    /* @id bstree_minimumAux */
    function minimumAux(root) {
        var node = root;
        while (node.leftCh !== undefined) {
            node = node.leftCh;
        }
        return node;
    }

    /**
     * Inserts the specified element into the tree if it's not already present.
     * @param {Object} element The element to insert.
     * @return {boolean} True if the tree didn't already contain the element.
     */
    /* @id bstree_add */
    tree.add = function (element) {
        if (buckets.isUndefined(element)) {
            return false;
        }

        /**
         * @private
         */
        /* @id bstree_add_insertNode */
        function insertNode(node) {
            var position = root,
                parent,
                cmp;

            while (position !== undefined) {
                cmp = compare(node.element, position.element);
                if (cmp === 0) {
                    return undefined;
                }
                if (cmp < 0) {
                    parent = position;
                    position = position.leftCh;
                } else {
                    parent = position;
                    position = position.rightCh;
                }
            }
            node.parent = parent;
            if (parent === undefined) {
                // tree is empty
                root = node;
            } else if (compare(node.element, parent.element) < 0) {
                parent.leftCh = node;
            } else {
                parent.rightCh = node;
            }
            return node;
        }

        var node = {
            element: element,
            leftCh: undefined,
            rightCh: undefined,
            parent: undefined
        };
        if (insertNode(node) !== undefined) {
            nElements += 1;
            return true;
        }
        return false;
    };

    /**
     * Removes all the elements from the tree.
     */
    /* @id bstree_clear */
    tree.clear = function () {
        root = undefined;
        nElements = 0;
    };

    /**
     * Returns true if the tree contains no elements.
     * @return {boolean} True if the tree contains no elements.
     */
    /* @id bstree_isEmpty */
    tree.isEmpty = function () {
        return nElements === 0;
    };

    /**
     * Returns the number of elements in the tree.
     * @return {number} The number of elements in the tree.
     */
    /* @id bstree_size */
    tree.size = function () {
        return nElements;
    };

    /**
     * Returns true if the tree contains the specified element.
     * @param {Object} element Element to search for.
     * @return {boolean} True if the tree contains the element,
     * false otherwise.
     */
    /* @id bstree_contains */
    tree.contains = function (element) {
        if (buckets.isUndefined(element)) {
            return false;
        }
        return searchNode(root, element) !== undefined;
    };

    /**
     * Removes the specified element from the tree.
     * @return {boolean} True if the tree contained the specified element.
     */
    /* @id bstree_remove */
    tree.remove = function (element) {
        var node;

        /* @id bstree_remove_transplant */
        function transplant(n1, n2) {
            if (n1.parent === undefined) {
                root = n2;
            } else if (n1 === n1.parent.leftCh) {
                n1.parent.leftCh = n2;
            } else {
                n1.parent.rightCh = n2;
            }
            if (n2 !== undefined) {
                n2.parent = n1.parent;
            }
        }
        /* @id bstree_remove_removeNode */
        function removeNode(node) {
            if (node.leftCh === undefined) {
                transplant(node, node.rightCh);
            } else if (node.rightCh === undefined) {
                transplant(node, node.leftCh);
            } else {
                var y = minimumAux(node.rightCh);
                if (y.parent !== node) {
                    transplant(y, y.rightCh);
                    y.rightCh = node.rightCh;
                    y.rightCh.parent = y;
                }
                transplant(node, y);
                y.leftCh = node.leftCh;
                y.leftCh.parent = y;
            }
        }

        node = searchNode(root, element);
        if (node === undefined) {
            return false;
        }
        removeNode(node);
        nElements -= 1;
        return true;
    };

    /**
     * Executes the provided function once per element present in the tree in in-order.
     * @param {function(Object):*} callback Function to execute, invoked with an element as 
     * argument. To break the iteration you can optionally return false in the callback.
     */
    /* @id bstree_inorderTraversal */
    tree.inorderTraversal = function (callback) {
        /* @id bstree_inorderTraversal_inorderRecursive */
        function inorderRecursive(node, callback, signal) {
            if (node === undefined || signal.stop) {
                return;
            }
            inorderRecursive(node.leftCh, callback, signal);
            if (signal.stop) {
                return;
            }
            signal.stop = callback(node.element) === false;
            if (signal.stop) {
                return;
            }
            inorderRecursive(node.rightCh, callback, signal);
        }

        inorderRecursive(root, callback, {
            stop: false
        });
    };

    /**
     * Executes the provided function once per element present in the tree in pre-order.
     * @param {function(Object):*} callback Function to execute, invoked with an element as 
     * argument. To break the iteration you can optionally return false in the callback.
     */
    /* @id bstree_preorderTraversal */
    tree.preorderTraversal = function (callback) {
        /* @id bstree_preorderTraversal_preorderRecursive */
        function preorderRecursive(node, callback, signal) {
            if (node === undefined || signal.stop) {
                return;
            }
            signal.stop = callback(node.element) === false;
            if (signal.stop) {
                return;
            }
            preorderRecursive(node.leftCh, callback, signal);
            if (signal.stop) {
                return;
            }
            preorderRecursive(node.rightCh, callback, signal);
        }

        preorderRecursive(root, callback, {
            stop: false
        });
    };

    /**
     * Executes the provided function once per element present in the tree in post-order.
     * @param {function(Object):*} callback Function to execute, invoked with an element as 
     * argument. To break the iteration you can optionally return false in the callback.
     */
    /* @id bstree_postorderTraversal */
    tree.postorderTraversal = function (callback) {
        /* @id bstree_postorderTraversal_postorderRecursive */
        function postorderRecursive(node, callback, signal) {
            if (node === undefined || signal.stop) {
                return;
            }
            postorderRecursive(node.leftCh, callback, signal);
            if (signal.stop) {
                return;
            }
            postorderRecursive(node.rightCh, callback, signal);
            if (signal.stop) {
                return;
            }
            signal.stop = callback(node.element) === false;
        }


        postorderRecursive(root, callback, {
            stop: false
        });
    };

    /**
     * Executes the provided function once per element present in the tree in level-order.
     * @param {function(Object):*} callback Function to execute, invoked with an element as 
     * argument. To break the iteration you can optionally return false in the callback.
     */
    /* @id bstree_levelTraversal */
    tree.levelTraversal = function (callback) {
        /* @id bstree_levelTraversal_levelAux */
        function levelAux(node, callback) {
            var queue = buckets.Queue();
            if (node !== undefined) {
                queue.enqueue(node);
            }
            while (!queue.isEmpty()) {
                node = queue.dequeue();
                if (callback(node.element) === false) {
                    return;
                }
                if (node.leftCh !== undefined) {
                    queue.enqueue(node.leftCh);
                }
                if (node.rightCh !== undefined) {
                    queue.enqueue(node.rightCh);
                }
            }
        }

        levelAux(root, callback);
    };

    /**
     * Returns the minimum element of the tree.
     * @return {*} The minimum element of the tree or undefined if the tree
     * is empty.
     */
    /* @id bstree_minimum */
    tree.minimum = function () {
        if (tree.isEmpty()) {
            return undefined;
        }
        return minimumAux(root).element;
    };

    /**
     * Returns the maximum element of the tree.
     * @return {*} The maximum element of the tree or undefined if the tree
     * is empty.
     */
    /* @id bstree_maximum */
    tree.maximum = function () {
        /* @id bstree_maximum_maximumAux */
        function maximumAux(node) {
            while (node.rightCh !== undefined) {
                node = node.rightCh;
            }
            return node;
        }

        if (tree.isEmpty()) {
            return undefined;
        }

        return maximumAux(root).element;
    };

    /**
     * Executes the provided function once per element present in the tree in in-order.
     * Equivalent to inorderTraversal.
     * @param {function(Object):*} callback Function to execute, it's
     * invoked with an element argument. To break the iteration you can
     * optionally return false in the callback.
     */
    /* @id bstree_forEach */
    tree.forEach = function (callback) {
        tree.inorderTraversal(callback);
    };

    /**
     * Returns an array containing all the elements in the tree in in-order.
     * @return {Array} An array containing all the elements in the tree in in-order.
     */
    /* @id bstree_toArray */
    tree.toArray = function () {
        var array = [];
        
        /* @id bstree_toArray_callback */
        tree.inorderTraversal(function (element) {
            array.push(element);
        });
        return array;
    };

    /**
     * Returns the height of the tree.
     * @return {number} The height of the tree or -1 if it's empty.
     */
    /* @id bstree_height */
    tree.height = function () {
        /* @id bstree_height_heightAux */
        function heightAux(node) {
            if (node === undefined) {
                return -1;
            }
            return Math.max(heightAux(node.leftCh), heightAux(node.rightCh)) + 1;
        }
        /* @id bstree_height_heightAux */
        function heightRecursive(node) {
            if (node === undefined) {
                return -1;
            }
            return Math.max(heightAux(node.leftCh), heightAux(node.rightCh)) + 1;
        }

        return heightRecursive(root);
    };

    /**
     * Returns true if the tree is equal to another tree.
     * Two trees are equal if they have the same elements.
     * @param {buckets.BSTree} other The other tree.
     * @return {boolean} True if the tree is equal to the given tree.
     */
    /* @id bstree_equals */
    tree.equals = function (other) {
        var isEqual;

        if (buckets.isUndefined(other) || typeof other.levelTraversal !== 'function') {
            return false;
        }
        if (tree.size() !== other.size()) {
            return false;
        }

        isEqual = true;
        
        /* @id bstree_equals_callback */
        other.forEach(function (element) {
            isEqual = tree.contains(element);
            return isEqual;
        });
        return isEqual;
    };

    return tree;
};

// ---------------------------- our tests now ---------------------------------

var bst = new buckets.BSTree();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

Assume(not (x1 = x2));
Assume(not (x1 = x3));
Assume(not (x2 = x3));

bst.add(x1);
bst.add(x2);
bst.add(x3);

// test 1: full traversal
var ar1 = [];
bst.inorderTraversal(function (x) {
  ar1.push(x);
});

var l = ar1.length;
Assert(l = 3);

var y1 = ar1[0];
var y2 = ar1[1];
var y3 = ar1[2];
Assert((y1 < y2) and (y2 < y3));
