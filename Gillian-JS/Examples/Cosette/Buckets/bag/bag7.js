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

// ------------------------------- dictionary.js -----------------------------

/**
 * Creates an empty dictionary.
 * @class <p>Dictionaries map keys to values, each key can map to at most one value.
 * This implementation accepts any kind of objects as keys.</p>
 *
 * <p>If the keys are custom objects, a function that converts keys to unique
 * strings must be provided at construction time.</p>
 * <p>Example:</p>
 * <pre>
 * function petToString(pet) {
 *  return pet.name;
 * }
 * </pre>
 * @constructor
 * @param {function(Object):string=} toStrFunction Optional function used
 * to convert keys to unique strings. If the keys aren't strings or if toString()
 * is not appropriate, a custom function which receives a key and returns a
 * unique string must be provided.
 */
buckets.Dictionary = function (toStrFunction) {

    /** 
     * @exports dictionary as buckets.Dictionary
     * @private
     */
    var dictionary = {},
        // Object holding the key-value pairs.
        table = {},
        // Number of keys in the dictionary.
        nElements = 0,
        // Function to convert keys unique to strings.
        toStr = toStrFunction || buckets.defaultToString,
        // Special string to prefix keys and avoid name collisions with existing properties.
        keyPrefix = '/$ ';

    /**
     * Returns the value associated with the specified key in the dictionary.
     * @param {Object} key The key.
     * @return {*} The mapped value or
     * undefined if the dictionary contains no mapping for the provided key.
     */
    dictionary.get = function (key) {
        var pair = table[keyPrefix + toStr(key)];
        if (buckets.isUndefined(pair)) {
            return undefined;
        }
        return pair.value;
    };

    /**
     * Associates the specified value with the specified key in the dictionary.
     * If the dictionary previously contained a mapping for the key, the old
     * value is replaced by the specified value.
     * @param {Object} key The key.
     * @param {Object} value Value to be mapped with the specified key.
     * @return {*} Previous value associated with the provided key, or undefined if
     * there was no mapping for the key or the key/value is undefined.
     */
    dictionary.set = function (key, value) {
        var ret, k, previousElement;
        if (buckets.isUndefined(key) || buckets.isUndefined(value)) {
            return undefined;
        }

        k = keyPrefix + toStr(key);
        previousElement = table[k];
        if (buckets.isUndefined(previousElement)) {
            nElements += 1;
            ret = undefined;
        } else {
            ret = previousElement.value;
        }
        table[k] = {
            key: key,
            value: value
        };
        return ret;
    };

    /**
     * Removes the value associated with the specified key from the dictionary if it exists.
     * @param {Object} key The key.
     * @return {*} Removed value associated with the specified key, or undefined if
     * there was no mapping for the key.
     */
    dictionary.remove = function (key) {
        var k = keyPrefix + toStr(key),
            previousElement = table[k];
        if (!buckets.isUndefined(previousElement)) {
            delete table[k];
            nElements -= 1;
            return previousElement.value;
        }
        return undefined;
    };

    /**
     * Returns an array containing all the keys in the dictionary.
     * @return {Array} An array containing all the keys in the dictionary.
     */
    dictionary.keys = function () {
        var array = [],
            name;
        for (name in table) {
            if (Object.prototype.hasOwnProperty.call(table, name)) {
                array.push(table[name].key);
            }
        }
        return array;
    };

    /**
     * Returns an array containing all the values in the dictionary.
     * @return {Array} An array containing all the values in the dictionary.
     */
    dictionary.values = function () {
        var array = [],
            name;
        for (name in table) {
            if (Object.prototype.hasOwnProperty.call(table, name)) {
                array.push(table[name].value);
            }
        }
        return array;
    };

    /**
     * Executes the provided function once per key-value pair
     * present in the dictionary.
     * @param {function(Object,Object):*} callback Function to execute. Receives
     * 2 arguments: key and value. To break the iteration you can
     * optionally return false inside the callback.
     */
    dictionary.forEach = function (callback) {
        var name, pair, ret;
        for (name in table) {
            if (Object.prototype.hasOwnProperty.call(table, name)) {
                pair = table[name];
                ret = callback(pair.key, pair.value);
                if (ret === false) {
                    return;
                }
            }
        }
    };

    /**
     * Returns true if the dictionary contains a mapping for the specified key.
     * @param {Object} key The key.
     * @return {boolean} True if the dictionary contains a mapping for the
     * specified key.
     */
    dictionary.containsKey = function (key) {
        return !buckets.isUndefined(dictionary.get(key));
    };

    /**
     * Removes all keys and values from the dictionary.
     * @this {buckets.Dictionary}
     */
    dictionary.clear = function () {
        table = {};
        nElements = 0;
    };

    /**
     * Returns the number of key-value pais in the dictionary.
     * @return {number} The number of key-value mappings in the dictionary.
     */
    dictionary.size = function () {
        return nElements;
    };

    /**
     * Returns true if the dictionary contains no keys.
     * @return {boolean} True if this dictionary contains no mappings.
     */
    dictionary.isEmpty = function () {
        return nElements <= 0;
    };

    /**
     * Returns true if the dictionary is equal to another dictionary.
     * Two dictionaries are equal if they have the same key-value pairs.
     * @param {buckets.Dictionary} other The other dictionary.
     * @param {function(Object,Object):boolean=} equalsFunction Optional
     * function to check if two values are equal. If the values in the dictionaries
     * are custom objects you should provide a custom equals function, otherwise
     * the === operator is used to check equality between values.
     * @return {boolean} True if the dictionary is equal to the given dictionary.
     */
    dictionary.equals = function (other, equalsFunction) {
        var eqf, isEqual;
        if (buckets.isUndefined(other) || typeof other.keys !== 'function') {
            return false;
        }
        if (dictionary.size() !== other.size()) {
            return false;
        }
        eqf = equalsFunction || buckets.defaultEquals;
        isEqual = true;
        other.forEach(function (k, v) {
            isEqual = eqf(dictionary.get(k), v);
            return isEqual;
        });
        return isEqual;
    };

    return dictionary;
};

// --------------------------------- arrays.js -------------------------------

// --------------------------------- arrays.js -------------------------------
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

// ---------------------------------- set.js ---------------------------------

/**
 * Creates an empty set.
 * @class <p>A set is a data structure that contains no duplicate items.</p>
 * <p>If the inserted elements are custom objects, a function
 * that converts elements to unique strings must be provided at construction time. 
 * <p>Example:</p>
 * <pre>
 * function petToString(pet) {
 *  return pet.type + ' ' + pet.name;
 * }
 * </pre>
 *
 * @param {function(Object):string=} toStringFunction Optional function used
 * to convert elements to unique strings. If the elements aren't strings or if toString()
 * is not appropriate, a custom function which receives an object and returns a
 * unique string must be provided.
 */
buckets.Set = function (toStringFunction) {

    /** 
     * @exports theSet as buckets.Set
     * @private
     */
    var theSet = {},
        // Underlying storage.
        dictionary = new buckets.Dictionary(toStringFunction);

    /**
     * Returns true if the set contains the specified element.
     * @param {Object} element Element to search for.
     * @return {boolean} True if the set contains the specified element,
     * false otherwise.
     */
    theSet.contains = function (element) {
        return dictionary.containsKey(element);
    };

    /**
     * Adds the specified element to the set if it's not already present.
     * @param {Object} element The element to insert.
     * @return {boolean} True if the set did not already contain the specified element.
     */
    theSet.add = function (element) {
        if (theSet.contains(element) || buckets.isUndefined(element)) {
            return false;
        }
        dictionary.set(element, element);
        return true;
    };

    /**
     * Performs an intersection between this and another set.
     * Removes all values that are not present in this set and the given set.
     * @param {buckets.Set} otherSet Other set.
     */
    theSet.intersection = function (otherSet) {
        theSet.forEach(function (element) {
            if (!otherSet.contains(element)) {
                theSet.remove(element);
            }
        });
    };

    /**
     * Performs a union between this and another set.
     * Adds all values from the given set to this set.
     * @param {buckets.Set} otherSet Other set.
     */
    theSet.union = function (otherSet) {
        otherSet.forEach(function (element) {
            theSet.add(element);
        });
    };

    /**
     * Performs a difference between this and another set.
     * Removes all the values that are present in the given set from this set.
     * @param {buckets.Set} otherSet other set.
     */
    theSet.difference = function (otherSet) {
        otherSet.forEach(function (element) {
            theSet.remove(element);
        });
    };

    /**
     * Checks whether the given set contains all the elements of this set.
     * @param {buckets.Set} otherSet Other set.
     * @return {boolean} True if this set is a subset of the given set.
     */
    theSet.isSubsetOf = function (otherSet) {
        var isSub = true;

        if (theSet.size() > otherSet.size()) {
            return false;
        }

        theSet.forEach(function (element) {
            if (!otherSet.contains(element)) {
                isSub = false;
                return false;
            }
        });
        return isSub;
    };

    /**
     * Removes the specified element from the set.
     * @return {boolean} True if the set contained the specified element, false
     * otherwise.
     */
    theSet.remove = function (element) {
        if (!theSet.contains(element)) {
            return false;
        }
        dictionary.remove(element);
        return true;
    };

    /**
     * Executes the provided function once per element
     * present in the set.
     * @param {function(Object):*} callback Function to execute, it's
     * invoked an element as argument. To break the iteration you can
     * optionally return false inside the callback.
     */
    theSet.forEach = function (callback) {
        dictionary.forEach(function (k, v) {
            return callback(v);
        });
    };

    /**
     * Returns an array containing all the elements in the set in no particular order.
     * @return {Array} An array containing all the elements in the set.
     */
    theSet.toArray = function () {
        return dictionary.values();
    };

    /**
     * Returns true if the set contains no elements.
     * @return {boolean} True if the set contains no elements.
     */
    theSet.isEmpty = function () {
        return dictionary.isEmpty();
    };

    /**
     * Returns the number of elements in the set.
     * @return {number} The number of elements in the set.
     */
    theSet.size = function () {
        return dictionary.size();
    };

    /**
     * Removes all the elements from the set.
     */
    theSet.clear = function () {
        dictionary.clear();
    };

    /**
     * Returns true if the set is equal to another set.
     * Two sets are equal if they have the same elements.
     * @param {buckets.Set} other The other set.
     * @return {boolean} True if the set is equal to the given set.
     */
    theSet.equals = function (other) {
        var isEqual;
        if (buckets.isUndefined(other) || typeof other.isSubsetOf !== 'function') {
            return false;
        }
        if (theSet.size() !== other.size()) {
            return false;
        }

        isEqual = true;
        other.forEach(function (element) {
            isEqual = theSet.contains(element);
            return isEqual;
        });
        return isEqual;
    };

    return theSet;
};

// ---------------------------------- bag.js ---------------------------------

/**
 * Creates an empty bag.
 * @class <p>A bag is a special kind of set in which members are
 * allowed to appear more than once.</p>
 * <p>If the inserted elements are custom objects, a function
 * that maps elements to unique strings must be provided at construction time.</p>
 * <p>Example:</p>
 * <pre>
 * function petToUniqueString(pet) {
 *  return pet.type + ' ' + pet.name;
 * }
 * </pre>
 *
 * @constructor
 * @param {function(Object):string=} toStrFunction Optional function
 * to convert elements to unique strings. If the elements aren't strings or if toString()
 * is not appropriate, a custom function which receives an object and returns a
 * unique string must be provided.
 */
buckets.Bag = function (toStrFunction) {

    /** 
     * @exports bag as buckets.Bag
     * @private
     */
    var bag = {},
        // Function to convert elements to unique strings.
        toStrF = toStrFunction || buckets.defaultToString,
        // Underlying  Storage
        dictionary = new buckets.Dictionary(toStrF),
        // Number of elements in the bag, including duplicates.
        nElements = 0;
    /**
     * Adds nCopies of the specified element to the bag.
     * @param {Object} element Element to add.
     * @param {number=} nCopies The number of copies to add, if this argument is
     * undefined 1 copy is added.
     * @return {boolean} True unless element is undefined.
     */
    /* @id bag_add */
    bag.add = function (element, nCopies) {
        var node;
        if (isNaN(nCopies) || buckets.isUndefined(nCopies)) {
            nCopies = 1;
        }
        if (buckets.isUndefined(element) || nCopies <= 0) {
            return false;
        }

        if (!bag.contains(element)) {
            node = {
                value: element,
                copies: nCopies
            };
            dictionary.set(element, node);
        } else {
            dictionary.get(element).copies += nCopies;
        }
        nElements += nCopies;
        return true;
    };

    /**
     * Counts the number of copies of the specified element in the bag.
     * @param {Object} element The element to search for.
     * @return {number} The number of copies of the element, 0 if not found.
     */
    /* @id bag_count */
    bag.count = function (element) {
        if (!bag.contains(element)) {
            return 0;
        }
        return dictionary.get(element).copies;
    };

    /**
     * Returns true if the bag contains the specified element.
     * @param {Object} element Element to search for.
     * @return {boolean} True if the bag contains the specified element,
     * false otherwise.
     */
    /* @id bag_contains */
    bag.contains = function (element) {
        return dictionary.containsKey(element);
    };

    /**
     * Removes nCopies of the specified element from the bag.
     * If the number of copies to remove is greater than the actual number
     * of copies in the bag, all copies are removed.
     * @param {Object} element Element to remove.
     * @param {number=} nCopies The number of copies to remove, if this argument is
     * undefined 1 copy is removed.
     * @return {boolean} True if at least 1 copy was removed.
     */
    /* @id bag_remove */
    bag.remove = function (element, nCopies) {
        var node;
        if (isNaN(nCopies) || buckets.isUndefined(nCopies)) {
            nCopies = 1;
        }
        if (buckets.isUndefined(element) || nCopies <= 0) {
            return false;
        }

        if (!bag.contains(element)) {
            return false;
        }
        node = dictionary.get(element);
        if (nCopies > node.copies) {
            nElements -= node.copies;
        } else {
            nElements -= nCopies;
        }
        node.copies -= nCopies;
        if (node.copies <= 0) {
            dictionary.remove(element);
        }
        return true;
    };

    /**
     * Returns an array containing all the elements in the bag in no particular order,
     * including multiple copies.
     * @return {Array} An array containing all the elements in the bag.
     */
    /* @id bag_toArray */
    bag.toArray = function () {
        var a = [],
            values = dictionary.values(),
            vl = values.length,
            node,
            element,
            copies,
            i,
            j;
        for (i = 0; i < vl; i += 1) {
            node = values[i];
            element = node.value;
            copies = node.copies;
            for (j = 0; j < copies; j += 1) {
                a.push(element);
            }
        }
        return a;
    };

    /**
     * Returns a set of unique elements in the bag.
     * @return {buckets.Set} A set of unique elements in the bag.
     */
    /* @id bag_toSet */
    bag.toSet = function () {
        var set = new buckets.Set(toStrF),
            elements = dictionary.values(),
            l = elements.length,
            i;
        for (i = 0; i < l; i += 1) {
            set.add(elements[i].value);
        }
        return set;
    };

    /**
     * Executes the provided function once per element
     * present in the bag, including multiple copies.
     * @param {function(Object):*} callback Function to execute, it's
     * invoked with an element as argument. To break the iteration you can
     * optionally return false in the callback.
     */
    /* @id bag_forEach */
    bag.forEach = function (callback) {
        /* @id bag_forEach_callback */
        dictionary.forEach(function (k, v) {
            var value = v.value,
                copies = v.copies,
                i;
            for (i = 0; i < copies; i += 1) {
                if (callback(value) === false) {
                    return false;
                }
            }
            return true;
        });
    };
    /**
     * Returns the number of elements in the bag, including duplicates.
     * @return {number} The number of elements in the bag.
     */
    /* @id bag_size */
    bag.size = function () {
        return nElements;
    };

    /**
     * Returns true if the bag contains no elements.
     * @return {boolean} True if the bag contains no elements.
     */
    /* @id bag_isEmpty */
    bag.isEmpty = function () {
        return nElements === 0;
    };

    /**
     * Removes all the elements from the bag.
     */
    /* @id bag_clear */
    bag.clear = function () {
        nElements = 0;
        dictionary.clear();
    };

    /**
     * Returns true if the bag is equal to another bag.
     * Two bags are equal if they have the same elements and
     * same number of copies per element.
     * @param {buckets.Bag} other The other bag.
     * @return {boolean} True if the bag is equal to the given bag.
     */
    /* @id bag_equals */
    bag.equals = function (other) {
        var isEqual;
        if (buckets.isUndefined(other) || typeof other.toSet !== 'function') {
            return false;
        }
        if (bag.size() !== other.size()) {
            return false;
        }

        isEqual = true;
        /* @id bag_equals_callback */
        other.forEach(function (element) {
            isEqual = (bag.count(element) === other.count(element));
            return isEqual;
        });
        return isEqual;
    };

    return bag;
};

// ------------------------------ our tests now ------------------------------

// init
var bag = new buckets.Bag();

// size
var n1 = symb_number();
var n2 = symb_number();
var n3 = symb_number();

bag.add(n1);
bag.add(n2);

var bag2 = new buckets.Bag();

var res1 = bag.equals(bag2);
Assert(not res1);

var res3 = bag.equals([n1, n2, n3]);
Assert(not res3);

bag.forEach(function(x) {
  if (x == n3) {
    return false;
  }
  bag2.add(x);
});

var res2 = bag.equals(bag2);
Assert(((not (n3 = n1)) and (not (n3 = n2)) and res2) or (((n3 = n1) or (n3 = n2)) and (not res2)));