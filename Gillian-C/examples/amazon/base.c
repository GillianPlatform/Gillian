#include "base.h"
#include <stdlib.h>

/*@
    import  `logic/EncryptionHeaderLogic`,
            `logic/ListLogic`,
            `logic/Utf8Logic`,
            `logic/ByteLogic`;
*/

/*
    Various useful predicates for memory management. For aws_mul_u64_checked, see
    https://github.com/awslabs/aws-c-common/blob/bb797381f3468e6f076e53eddbb399a99f54f67b/include/aws/common/math.fallback.inl#L30
    Our predicate describes the valid case.
*/
/*@
    pred aws_add_u64_checked(+a, +b, out) {
        (out == (a + b)) *
        (out <# 4611686018427387903)
    }

    pred aws_mul_u64_checked(+a, +b, out) {
        (out == (a * b)) *
        (out <# 4611686018427387903)
    }

    pred nounfold writable_memory(+pointer, +length, content) {
        (length == 0) * (content == []);
        (0 <# length) * ARRAY(pointer, char, length, content)
    }

    pred nounfold optBytes(+bytes, +length, content) {
        (length == 0) * (content == nil);
        (0 <# length) * ARRAY(bytes, char, length, content)
    }

    lemma optBytesConcat(lptr, llength, rptr, rlength) {
        hypothesis:
            optBytes(#lptr, #llength, #lcont) * optBytes(#rptr, #rlength, #rcont) *
            (#rptr == #lptr p+ #llength)

        conclusions:
            optBytes(#lptr, #llength + #rlength, #lcont @ #rcont)

        proof:
            unfold optBytes(#lptr, #llength, #lcont);
            unfold optBytes(#rptr, #rlength, #rcont);
            fold optBytes(#lptr, #llength + #rlength, #lcont @ #rcont)
    }
*/

int aws_add_size_checked(size_t a, size_t b, size_t *res) {
    if (a > 65535 || b > 65535) {
        GILLIAN("assert False");
        return 1;
    } else {
        *res = a + b;
        return 0;
    }
}

int aws_mul_size_checked(size_t a, size_t b, size_t *res) {
    if (a > 65535 || b > 65535) {
        GILLIAN("assert False");
        return 0;
    } else {
        *res = a * b;
        return 0;
    }
}
