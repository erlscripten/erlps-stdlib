
test: build_tests
	spago test

BEAM_PATH = /usr/lib/erlang/lib/stdlib-3.13.2/ebin
build_stdlib:
	./erlscripten -s $(BEAM_PATH)/array.beam -o src/Array.purs
	./erlscripten -s $(BEAM_PATH)/queue.beam -o src/Queue.purs
	./erlscripten -s $(BEAM_PATH)/dict.beam -o src/Dict.purs
	./erlscripten -s $(BEAM_PATH)/lists.beam -o src/Lists.purs
	./erlscripten -s $(BEAM_PATH)/gb_sets.beam -o src/GBSets.purs
	./erlscripten -s $(BEAM_PATH)/gb_trees.beam -o src/GBTrees.purs
	./erlscripten -s $(BEAM_PATH)/maps.beam -o src/Maps.purs
	./erlscripten -s $(BEAM_PATH)/orddict.beam -o src/OrdDict.purs
	./erlscripten -s $(BEAM_PATH)/ordsets.beam -o src/OrdSets.purs
	./erlscripten -s $(BEAM_PATH)/proplists.beam -o src/Proplists.purs
	./erlscripten -s $(BEAM_PATH)/sets.beam -o src/Sets.purs
	./erlscripten -s $(BEAM_PATH)/string.beam -o src/String.purs
	./erlscripten -s $(BEAM_PATH)/digraph_utils.beam -o src/DigraphUtils.purs
	./erlscripten -s $(BEAM_PATH)/digraph.beam -o src/Digraph.purs
	./erlscripten -s $(BEAM_PATH)/rand.beam -o src/Rand.purs
	./erlscripten -s $(BEAM_PATH)/sofs.beam -o src/Sofs.purs
	./erlscripten -s $(BEAM_PATH)/erl_parse.beam -o src/Erl.Parse.purs
	./erlscripten -s $(BEAM_PATH)/erl_anno.beam -o src/Erl.Anno.purs
	./erlscripten -s $(BEAM_PATH)/erl_eval.beam -o src/Erl.Eval.purs
	./erlscripten -s $(BEAM_PATH)/erl_bits.beam -o src/Erl.Bits.purs
	./erlscripten -s $(BEAM_PATH)/erl_pp.beam -o src/Erl.Pp.purs
	./erlscripten -s $(BEAM_PATH)/erl_internal.beam -o src/Erl.Internal.purs

	./erlscripten -s $(BEAM_PATH)/io_lib.beam -o src/IO.Lib.purs
	./erlscripten -s $(BEAM_PATH)/io_lib_format.beam -o src/IO.Lib.Format.purs
	./erlscripten -s $(BEAM_PATH)/io_lib_pretty.beam -o src/IO.Lib.Pretty.purs

	erlc +debug_info erlang_src/erl_scan.erl
	./erlscripten -s erl_scan.beam -o src/Erl.Scan.purs
	rm erl_scan.beam

	erlc +debug_info erlang_src/erl_lint.erl
	./erlscripten -s erl_lint.beam -o src/Erl.Lint.purs
	rm erl_lint.beam

	erlc +debug_info erlang_src/unicode_util.erl
	./erlscripten -s unicode_util.beam -o src/Unicode.Util.purs
	rm unicode_util.beam

	erlc +debug_info erlang_src/epp.erl
	./erlscripten -s epp.beam -o src/Epp.purs
	rm epp.beam

build_tests:
	erlc +debug_info erlang_tests/array_SUITE.erl
	./erlscripten -s array_SUITE.beam -o test/Array.SUITE.purs
	rm array_SUITE.beam

	erlc +debug_info erlang_tests/maps_SUITE.erl
	./erlscripten -s maps_SUITE.beam -o test/Maps.SUITE.purs
	rm maps_SUITE.beam

	erlc +debug_info erlang_tests/lists_SUITE.erl
	./erlscripten -s lists_SUITE.beam -o test/Lists.SUITE.purs
	rm lists_SUITE.beam

	erlc +debug_info erlang_tests/queue_SUITE.erl
	./erlscripten -s queue_SUITE.beam -o test/Queue.SUITE.purs
	rm queue_SUITE.beam

	erlc +debug_info erlang_tests/dict_SUITE.erl
	./erlscripten -s dict_SUITE.beam -o test/Dict.SUITE.purs
	rm dict_SUITE.beam

	erlc +debug_info erlang_tests/dict_test_lib.erl
	./erlscripten -s dict_test_lib.beam -o test/Dict.Test.Lib.purs
	rm dict_test_lib.beam

	erlc +debug_info erlang_tests/proplists_SUITE.erl
	./erlscripten -s proplists_SUITE.beam -o test/Proplists.SUITE.purs
	rm proplists_SUITE.beam

	erlc +debug_info erlang_tests/sets_SUITE.erl
	./erlscripten -s sets_SUITE.beam -o test/Sets.SUITE.purs
	rm sets_SUITE.beam

	erlc +debug_info erlang_tests/sets_test_lib.erl
	./erlscripten -s sets_test_lib.beam -o test/Sets.Test.Lib.purs
	rm sets_test_lib.beam

	erlc +debug_info erlang_tests/math_SUITE.erl
	./erlscripten -s math_SUITE.beam -o test/Math.SUITE.purs
	rm math_SUITE.beam

	erlc +debug_info erlang_tests/sofs_SUITE.erl
	./erlscripten -s sofs_SUITE.beam -o test/Sofs.SUITE.purs
	rm sofs_SUITE.beam

	erlc +debug_info erlang_tests/erl_scan_SUITE.erl
	./erlscripten -s erl_scan_SUITE.beam -o test/Erl.Scan.SUITE.purs
	rm erl_scan_SUITE.beam

	erlc +debug_info erlang_tests/erl_eval_SUITE.erl
	./erlscripten -s erl_eval_SUITE.beam -o test/Erl.Eval.SUITE.purs
	rm erl_eval_SUITE.beam

	erlc +debug_info erlang_tests/erl_anno_SUITE.erl
	./erlscripten -s erl_anno_SUITE.beam -o test/Erl.Anno.SUITE.purs
	rm erl_anno_SUITE.beam

