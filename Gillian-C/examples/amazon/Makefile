ifdef GITHUB_ACTIONS
	VERIFY=gillian-c verify
else
	VERIFY=opam exec -- dune exec -- gillian-c verify
endif

default:
	${VERIFY} \
		header.c edk.c array_list.c ec.c byte_buf.c \
		hash_table.c string.c allocator.c \
		error.c base.c \
		--fstruct-passing --no-lemma-proof -l disabled

proc:
	${VERIFY} \
		header.c edk.c array_list.c ec.c byte_buf.c \
		hash_table.c string.c allocator.c \
		error.c base.c \
		--fstruct-passing --no-lemma-proof --proc $(PROC) -l disabled

lemma:
	${VERIFY} \
		header.c edk.c array_list.c ec.c byte_buf.c \
		hash_table.c string.c allocator.c \
		error.c base.c \
		--fstruct-passing --no-lemma-proof --lemma $(LEMMA) -l disabled

string-bug:
	-${VERIFY} \
		error.c bugs/string.c allocator.c base.c \
		--no-lemma-proof --proc aws_string_new_from_array -l disabled

header-bug:
	-${VERIFY} \
		bugs/header.c edk.c array_list.c ec.c \
		byte_buf.c hash_table.c string.c allocator.c error.c \
		bugs/base.c \
		--fstruct-passing --no-lemma-proof --proc aws_cryptosdk_hdr_parse -l disabled

byte-cursor-ub:
	-${VERIFY} \
		bugs/byte_buf.c allocator.c \
		error.c base.c \
		--fstruct-passing --no-lemma-proof --proc aws_byte_cursor_advance -l disabled
		
bugs: string-bug header-bug byte-cursor-ub

clean:
	rm -rf *.i *.deps