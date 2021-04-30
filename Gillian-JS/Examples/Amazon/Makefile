default:
	esy x gillian-js verify deserialize_factory.js --no-lemma-proof -l disabled

pp-bug:
	-esy x gillian-js verify bugs/pp/deserialize_factory.js --no-lemma-proof -l normal

frozen-bug:
	-esy x gillian-js verify bugs/frozen/deserialize_factory.js --no-lemma-proof
	
bugs: pp-bug frozen-bug