string-bug:
	esy x gillian-c verify \
		string.c \
		../allocator.c \
		../base.c --no-lemma-proof

header-bug:
	esy x gillian-c verify \
		header.c \
		../edk.c \
		../array_list.c \
		../ec.c \
		../byte_buf.c \
		../hash_table.c \
		../string.c \
		../allocator.c \
		../error.c \
		../base.c \
		--fstruct-passing --no-lemma-proof --proc aws_cryptosdk_hdr_parse -l disabled