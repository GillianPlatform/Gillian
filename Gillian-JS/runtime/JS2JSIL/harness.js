// Copyright (C) 2017 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Verify that a subArray is contained within an array.
---*/

/**
 * @param {Array} array
 * @param {Array} subArray
 */

function arrayContains(array, subArray) {
    var found;
    for (var i = 0; i < subArray.length; i++) {
        found = false;
        for (var j = 0; j < array.length; j++) {
            if (subArray[i] === array[j]) {
                found = true;
                break;
            }
        }
        if (!found) {
            return false;
        }
    }
    return true;
}

// Copyright (C) 2017 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Collection of assertion functions used throughout test262
---*/

function assert(mustBeTrue, message) {
    if (mustBeTrue === true) {
        return;
    }

    if (message === undefined) {
        message = 'Expected true but got ' + String(mustBeTrue);
    }
    $ERROR(message);
}

assert._isSameValue = function (a, b) {
    if (a === b) {
        // Handle +/-0 vs. -/+0
        return a !== 0 || 1 / a === 1 / b;
    }

    // Handle NaN vs. NaN
    return a !== a && b !== b;
};

assert.sameValue = function (actual, expected, message) {
    try {
        if (assert._isSameValue(actual, expected)) {
            return;
        }
    } catch (error) {
        $ERROR(message + ' (_isSameValue operation threw) ' + error);
        return;
    }

    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    message += 'Expected SameValue(«' + String(actual) + '», «' + String(expected) + '») to be true';

    $ERROR(message);
};

assert.notSameValue = function (actual, unexpected, message) {
    if (!assert._isSameValue(actual, unexpected)) {
        return;
    }

    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    message += 'Expected SameValue(«' + String(actual) + '», «' + String(unexpected) + '») to be false';

    $ERROR(message);
};

assert.throws = function (expectedErrorConstructor, func, message) {
    if (typeof func !== "function") {
        $ERROR('assert.throws requires two arguments: the error constructor ' +
            'and a function to run');
        return;
    }
    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    try {
        func();
    } catch (thrown) {
        if (typeof thrown !== 'object' || thrown === null) {
            message += 'Thrown value was not an object!';
            $ERROR(message);
        } else if (thrown.constructor !== expectedErrorConstructor) {
            message += 'Expected a ' + expectedErrorConstructor.name + ' but got a ' + thrown.constructor.name;
            $ERROR(message);
        }
        return;
    }

    message += 'Expected a ' + expectedErrorConstructor.name + ' to be thrown but no exception was thrown at all';
    $ERROR(message);
};

// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Verify that the given date object's Number representation describes the
    correct number of milliseconds since the Unix epoch relative to the local
    time zone (as interpreted at the specified date).
---*/

/**
 * @param {Date} date
 * @param {Number} expectedMs
 */
function assertRelativeDateMs(date, expectedMs) {
    var actualMs = date.valueOf();
    var localOffset = date.getTimezoneOffset() * 60000;

    if (actualMs - localOffset !== expectedMs) {
        $ERROR(
            'Expected ' + date + ' to be ' + expectedMs +
            ' milliseconds from the Unix epoch'
        );
    }
}

// Copyright (C) 2017 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Compare the contents of two arrays
---*/

function compareArray(a, b) {
    if (b.length !== a.length) {
        return false;
    }

    for (var i = 0; i < a.length; i++) {
        if (b[i] !== a[i]) {
            return false;
        }
    }
    return true;
}

assert.compareArray = function (actual, expected, message) {
    assert(compareArray(actual, expected),
        'Expected [' + actual.join(', ') + '] and [' + expected.join(', ') + '] to have the same contents. ' + message);
};

// Copyright (C) 2009 the Sputnik authors.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Collection of date-centric values
---*/

var date_1899_end = -2208988800001;
var date_1900_start = -2208988800000;
var date_1969_end = -1;
var date_1970_start = 0;
var date_1999_end = 946684799999;
var date_2000_start = 946684800000;
var date_2099_end = 4102444799999;
var date_2100_start = 4102444800000;

var start_of_time = -8.64e15;
var end_of_time = 8.64e15;

// Copyright (C) 2017 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Collection of functions used to safely verify the correctness of
    property descriptors.
---*/

function verifyProperty(obj, name, desc, options) {
    assert(
        arguments.length > 2,
        'verifyProperty should receive at least 3 arguments: obj, name, and descriptor'
    );

    var originalDesc = Object.getOwnPropertyDescriptor(obj, name);
    var nameStr = String(name);

    // Allows checking for undefined descriptor if it's explicitly given.
    if (desc === undefined) {
        assert.sameValue(
            originalDesc,
            undefined,
            "obj['" + nameStr + "'] descriptor should be undefined"
        );

        // desc and originalDesc are both undefined, problem solved;
        return true;
    }

    assert(
        Object.prototype.hasOwnProperty.call(obj, name),
        "obj should have an own property " + nameStr
    );

    assert.notSameValue(
        desc,
        null,
        "The desc argument should be an object or undefined, null"
    );

    assert.sameValue(
        typeof desc,
        "object",
        "The desc argument should be an object or undefined, " + String(desc)
    );

    var failures = [];

    if (Object.prototype.hasOwnProperty.call(desc, 'value')) {
        if (desc.value !== originalDesc.value) {
            failures.push("descriptor value should be " + desc.value);
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'enumerable')) {
        if (desc.enumerable !== originalDesc.enumerable ||
            desc.enumerable !== isEnumerable(obj, name)) {
            failures.push('descriptor should ' + (desc.enumerable ? '' : 'not ') + 'be enumerable');
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'writable')) {
        if (desc.writable !== originalDesc.writable ||
            desc.writable !== isWritable(obj, name)) {
            failures.push('descriptor should ' + (desc.writable ? '' : 'not ') + 'be writable');
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'configurable')) {
        if (desc.configurable !== originalDesc.configurable ||
            desc.configurable !== isConfigurable(obj, name)) {
            failures.push('descriptor should ' + (desc.configurable ? '' : 'not ') + 'be configurable');
        }
    }

    assert(!failures.length, failures.join('; '));

    if (options && options.restore) {
        Object.defineProperty(obj, name, originalDesc);
    }

    return true;
}

function isConfigurable(obj, name) {
    try {
        delete obj[name];
    } catch (e) {
        if (!(e instanceof TypeError)) {
            $ERROR("Expected TypeError, got " + e);
        }
    }
    return !Object.prototype.hasOwnProperty.call(obj, name);
}

function isEnumerable(obj, name) {
    var stringCheck = false;

    if (typeof name === "string") {
        for (var x in obj) {
            if (x === name) {
                stringCheck = true;
                break;
            }
        }
    } else {
        // skip it if name is not string, works for Symbol names.
        stringCheck = true;
    }

    return stringCheck &&
        Object.prototype.hasOwnProperty.call(obj, name) &&
        Object.prototype.propertyIsEnumerable.call(obj, name);
}

function isEqualTo(obj, name, expectedValue) {
    var actualValue = obj[name];

    return assert._isSameValue(actualValue, expectedValue);
}

function isWritable(obj, name, verifyProp, value) {
    var newValue = value || "unlikelyValue";
    var hadValue = Object.prototype.hasOwnProperty.call(obj, name);
    var oldValue = obj[name];
    var writeSucceeded;

    try {
        obj[name] = newValue;
    } catch (e) {
        if (!(e instanceof TypeError)) {
            $ERROR("Expected TypeError, got " + e);
        }
    }

    writeSucceeded = isEqualTo(obj, verifyProp || name, newValue);

    // Revert the change only if it was successful (in other cases, reverting
    // is unnecessary and may trigger exceptions for certain property
    // configurations)
    if (writeSucceeded) {
        if (hadValue) {
            obj[name] = oldValue;
        } else {
            delete obj[name];
        }
    }

    return writeSucceeded;
}

function verifyEqualTo(obj, name, value) {
    if (!isEqualTo(obj, name, value)) {
        $ERROR("Expected obj[" + String(name) + "] to equal " + value +
            ", actually " + obj[name]);
    }
}

function verifyWritable(obj, name, verifyProp, value) {
    if (!verifyProp) {
        assert(Object.getOwnPropertyDescriptor(obj, name).writable,
            "Expected obj[" + String(name) + "] to have writable:true.");
    }
    if (!isWritable(obj, name, verifyProp, value)) {
        $ERROR("Expected obj[" + String(name) + "] to be writable, but was not.");
    }
}

function verifyNotWritable(obj, name, verifyProp, value) {
    if (!verifyProp) {
        assert(!Object.getOwnPropertyDescriptor(obj, name).writable,
            "Expected obj[" + String(name) + "] to have writable:false.");
    }
    if (isWritable(obj, name, verifyProp)) {
        $ERROR("Expected obj[" + String(name) + "] NOT to be writable, but was.");
    }
}

function verifyEnumerable(obj, name) {
    assert(Object.getOwnPropertyDescriptor(obj, name).enumerable,
        "Expected obj[" + String(name) + "] to have enumerable:true.");
    if (!isEnumerable(obj, name)) {
        $ERROR("Expected obj[" + String(name) + "] to be enumerable, but was not.");
    }
}

function verifyNotEnumerable(obj, name) {
    assert(!Object.getOwnPropertyDescriptor(obj, name).enumerable,
        "Expected obj[" + String(name) + "] to have enumerable:false.");
    if (isEnumerable(obj, name)) {
        $ERROR("Expected obj[" + String(name) + "] NOT to be enumerable, but was.");
    }
}

function verifyConfigurable(obj, name) {
    assert(Object.getOwnPropertyDescriptor(obj, name).configurable,
        "Expected obj[" + String(name) + "] to have configurable:true.");
    if (!isConfigurable(obj, name)) {
        $ERROR("Expected obj[" + String(name) + "] to be configurable, but was not.");
    }
}

function verifyNotConfigurable(obj, name) {
    assert(!Object.getOwnPropertyDescriptor(obj, name).configurable,
        "Expected obj[" + String(name) + "] to have configurable:false.");
    if (isConfigurable(obj, name)) {
        $ERROR("Expected obj[" + String(name) + "] NOT to be configurable, but was.");
    }
}

// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Provides both:

    - An error class to avoid false positives when testing for thrown exceptions
    - A function to explicitly throw an exception using the Test262Error class
---*/


function Test262Error(message) {
    this.message = message || "";
}

Test262Error.prototype.toString = function () {
    return "Test262Error: " + this.message;
};

var $ERROR;
$ERROR = function $ERROR(message) {
    throw new Test262Error(message);
};

function $DONOTEVALUATE() {
    throw "Test262: This statement should not be evaluated.";
}

function decimalToHexString(n) {
    var hex = "0123456789ABCDEF";
    n >>>= 0;
    var s = "";
    while (n) {
      s = hex[n & 0xf] + s;
      n >>>= 4;
    }
    while (s.length < 4) {
      s = "0" + s;
    }
    return s;
  }
  
function decimalToPercentHexString(n) {
    var hex = "0123456789ABCDEF";
    return "%" + hex[(n >> 4) & 0xf] + hex[n & 0xf];
}