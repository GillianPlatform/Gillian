ifdef GITHUB_ACTIONS
	VERIFY=gillian-js verify
else
	VERIFY=dune exec -- gillian-js verify
endif

default:
	${VERIFY} deserialize_factory.js --no-lemma-proof -l disabled

pp-bug:
	-${VERIFY} bugs/pp/deserialize_factory.js --no-lemma-proof -l normal

frozen-bug:
	-${VERIFY} bugs/frozen/deserialize_factory.js --no-lemma-proof
	
bugs: pp-bug frozen-bug