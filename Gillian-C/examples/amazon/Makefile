default:
	esy x gillian-c verify \
		header.c edk.c array_list.c ec.c byte_buf.c \
		hash_table.c string.c allocator.c \
		error.c base.c \
		--fstruct-passing --no-lemma-proof -l disabled

proc:
	esy x gillian-c verify \
		header.c edk.c array_list.c ec.c byte_buf.c \
		hash_table.c string.c allocator.c \
		error.c base.c \
		--fstruct-passing --no-lemma-proof --proc $(PROC) -l disabled

lemma:
	esy x gillian-c verify \
		header.c edk.c array_list.c ec.c byte_buf.c \
		hash_table.c string.c allocator.c \
		error.c base.c \
		--fstruct-passing --no-lemma-proof --lemma $(LEMMA) -l disabled

string-bug:
	-esy x gillian-c verify \
		error.c bugs/string.c allocator.c base.c \
		--no-lemma-proof --proc aws_string_new_from_array -l disabled

header-bug:
	-esy x gillian-c verify \
		bugs/header.c edk.c array_list.c ec.c \
		byte_buf.c hash_table.c string.c allocator.c error.c \
		bugs/base.c \
		--fstruct-passing --no-lemma-proof --proc aws_cryptosdk_hdr_parse -l disabled

byte-cursor-ub:
	-esy x gillian-c verify \
		bugs/byte_buf.c allocator.c \
		error.c base.c \
		--fstruct-passing --no-lemma-proof --proc aws_byte_cursor_advance -l disabled
		
bugs: string-bug header-bug byte-cursor-ub