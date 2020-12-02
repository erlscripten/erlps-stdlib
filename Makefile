
test: build_tests
	spago test

BEAM_PATH = /usr/lib/erlang/lib/stdlib-3.13.2/ebin
build_stdlib:
	./erlscripten -s $(BEAM_PATH)/array.beam -o src/Array.purs
	#./erlscripten -s $(BEAM_PATH)/queue.beam -o src/Queue.purs
	#./erlscripten -s $(BEAM_PATH)/dict.beam -o src/Dict.purs
	./erlscripten -s $(BEAM_PATH)/lists.beam -o src/Lists.purs
	./erlscripten -s $(BEAM_PATH)/gb_sets.beam -o src/GBSets.purs
	./erlscripten -s $(BEAM_PATH)/gb_trees.beam -o src/GBTrees.purs
	./erlscripten -s $(BEAM_PATH)/maps.beam -o src/Maps.purs
	./erlscripten -s $(BEAM_PATH)/orddict.beam -o src/OrdDict.purs
	./erlscripten -s $(BEAM_PATH)/ordsets.beam -o src/OrdSets.purs
	./erlscripten -s $(BEAM_PATH)/proplists.beam -o src/Proplists.purs
	./erlscripten -s $(BEAM_PATH)/sets.beam -o src/Sets.purs
	#./erlscripten -s $(BEAM_PATH)/string.beam -o src/String.purs
	#./erlscripten -s $(BEAM_PATH)/digraph_utils.beam -o src/DigraphUtils.purs
	#./erlscripten -s $(BEAM_PATH)/digraph.beam -o src/Digraph.purs

build_tests:
	erlc +debug_info erlang_tests/array_SUITE.erl
	./erlscripten -s array_SUITE.beam -o test/Array.SUITE.purs
	rm array_SUITE.beam

	erlc +debug_info erlang_tests/maps_SUITE.erl
	./erlscripten -s maps_SUITE.beam -o test/Maps.SUITE.purs
	rm maps_SUITE.beam

	#erlc +debug_info erlang_tests/lists_SUITE.erl
	#./erlscripten -s lists_SUITE.beam -o test/Lists.SUITE.purs
	#rm lists_SUITE.beam

	#erlc +debug_info erlang_tests/queue_SUITE.erl
	#./erlscripten -s queue_SUITE.beam -o test/Queue.SUITE.purs
	#rm queue_SUITE.beam

	#%erlc +debug_info erlang_tests/dict_SUITE.erl
	#./erlscripten -s dict_SUITE.beam -o test/Dict.SUITE.purs
	#rm dict_SUITE.beam

	#erlc +debug_info erlang_tests/dict_test_lib.erl
	#./erlscripten -s dict_test_lib.beam -o test/Dict.Test.Lib.purs
	#rm dict_test_lib.beam
