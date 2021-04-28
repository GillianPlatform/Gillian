#include "hash_table.h"

// Axiomatic hashtables
/*@
    import `logic/hash_table_ax`;

    pred valid_hash_table_fields(+fields, alloc, rawContents, contents) {
        (fields == [ #p_impl ]) * (not (#p_impl == NULL)) *
        toUtf8PairMap(rawContents, contents) *
        axiomatic_hash_table_in_memory(#p_impl, alloc, rawContents, contents)
    }

    pred empty_hash_table_fields(+fields, alloc) {
        valid_hash_table_fields(fields, alloc, [], [])
    }

    pred nounfold valid_hash_table_ptr(+hash, alloc, rawContents, contents) {
        (hash -> struct aws_hash_table { #impl }) *
        (not (#impl == NULL)) *
        toUtf8PairMap(rawContents, contents) *
        axiomatic_hash_table_in_memory(#impl, alloc, rawContents, contents)
    }

    pred empty_hash_table_ptr(+hash, alloc) {
        valid_hash_table_ptr(hash, alloc, nil, nil)
    }
*/

// Axiomatic spec of aws_hash_table_clear(hash), which
// clears the hashtable hash
/*@
    axiomatic spec aws_hash_table_clear(hash) {
        requires:
            (hash == #hash) *
            valid_hash_table_ptr(#hash, #alloc, #rawContents, #contents) *
            default_allocator(#alloc)

        ensures:
            empty_hash_table_ptr(#hash, #alloc) *
            default_allocator(#alloc)
    }
*/

// Axiomatic spec of aws_hash_table_put(map, key, value, was_created), which
// puts the key key with value value into the map map, returning an indicator
// was_created, which is true if and only of the key was previously in the map
/*@
    axiomatic spec aws_hash_table_put(map, key, value, was_created) {
        requires:
            (map == #map) * (key == #key) * (value == #value) * (was_created == #wc) *
            valid_aws_string_ptr(#key, #alloc, #rawKeyContent, #keyContent) *
            valid_aws_string_ptr(#value, #alloc, #rawValueContent, #valueContent) *
            valid_hash_table_ptr(map, #alloc, #mapRawContent, #mapContent) *
            FirstProj(#mapContent, #keys) *
            ListToSet(#keys, #keySet) *
            ARRAY(#wc, int, 1, [ #trash ])

        ensures:
            valid_hash_table_ptr(#map, #alloc, #mapRawContent @ [ [#rawKeyContent, #rawValueContent] ], #mapContent @ [ [#keyContent, #valueContent] ]) *
            (not (#keyContent --e-- #keySet)) *
            (#wc -> int(1)) *
            (ret == int(0));

            valid_hash_table_ptr(#map, #alloc, #newRawContent, #newContent) *
            (#keyContent --e-- #keySet) *
            (#wc -> int(0)) *
            (ret == int(0))
    }
*/
void __for_aws_hash_table_to_appear(struct aws_hash_table *map) { (void) map; }