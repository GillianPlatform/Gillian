c:
	esy x gillian-c verify \
		Gillian-C/examples/amazon/header.c \
		Gillian-C/examples/amazon/edk.c \
		Gillian-C/examples/amazon/array_list.c \
		Gillian-C/examples/amazon/ec.c \
		Gillian-C/examples/amazon/byte_buf.c \
		Gillian-C/examples/amazon/hash_table.c \
		Gillian-C/examples/amazon/string.c \
		Gillian-C/examples/amazon/allocator.c \
		Gillian-C/examples/amazon/error.c \
		Gillian-C/examples/amazon/base.c \
		--fstruct-passing --no-lemma-proof -l disabled

c-proc:
	esy x gillian-c verify \
		Gillian-C/examples/amazon/header.c \
		Gillian-C/examples/amazon/edk.c \
		Gillian-C/examples/amazon/array_list.c \
		Gillian-C/examples/amazon/ec.c \
		Gillian-C/examples/amazon/byte_buf.c \
		Gillian-C/examples/amazon/hash_table.c \
		Gillian-C/examples/amazon/string.c \
		Gillian-C/examples/amazon/allocator.c \
		Gillian-C/examples/amazon/error.c \
		Gillian-C/examples/amazon/base.c \
		--fstruct-passing --no-lemma-proof --proc $(PROC) -l disabled

c-lemma:
	esy x gillian-c verify \
		Gillian-C/examples/amazon/header.c \
		Gillian-C/examples/amazon/edk.c \
		Gillian-C/examples/amazon/array_list.c \
		Gillian-C/examples/amazon/ec.c \
		Gillian-C/examples/amazon/byte_buf.c \
		Gillian-C/examples/amazon/hash_table.c \
		Gillian-C/examples/amazon/string.c \
		Gillian-C/examples/amazon/allocator.c \
		Gillian-C/examples/amazon/error.c \
		Gillian-C/examples/amazon/base.c \
		--fstruct-passing --no-lemma-proof --lemma $(LEMMA) -l disabled

js:
	esy x gillian-js verify Gillian-JS/Examples/Amazon/deserialize_factory.js --no-lemma-proof -l disabled

js-pp-bug:
	esy x gillian-js verify Gillian-JS/Examples/Amazon/bugs/pp/deserialize_factory.js --no-lemma-proof -l normal

js-frozen-bug:
	esy x gillian-js verify Gillian-JS/Examples/Amazon/bugs/frozen/deserialize_factory.js --no-lemma-proof