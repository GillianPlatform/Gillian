ifdef GITHUB_ACTIONS
	ACT=gillian-c act
else
	ACT=esy x gillian-c act
endif

default:
	${ACT} \
		header.c edk.c array_list.c ec.c byte_buf.c \
		hash_table.c string.c \
		error.c base.c \
		--fstruct-passing --ignore-undef --specs-to-stdout -l disabled

clean:
	rm -rf *.i *.deps