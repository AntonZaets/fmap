fmap
=====

Simple replacement for standard `map` module for case when all possible keys are known and there aren't too many such keys. This implementation is just an experiment for comparing with map performance.
Doc is skipped for now, the difference from the usual `map`:
- object should be build with `fmap:new` which expects list of all possible keys and value that will indicate unexistence of the key;
- all function that returns key in map returns tuple `{Key :: any(), Index :: pos_integer()}`;
- all function that expect keys expects indexes

Example of usage:

```erlang

1> fmap:new([user, email, address], '$undef').
{[1,2,3],
 {fmap,[user,email,address],
       '$undef',
       {'$undef','$undef','$undef'},
       0}}
2> {Enumerated, FMap} = fmap:new([user, email, address], '$undef').
{[1,2,3],
 {fmap,[user,email,address],
       '$undef',
       {'$undef','$undef','$undef'},
       0}}
3> [ fmap:get(I, FMap) || I <- Enumerated].
** exception throw: {badkey,1}
     in function  fmap:do_get/3 (/home/anton/Documents/programming/fmap/_build/default/lib/fmap/src/fmap.erl, line 209)
4> [ fmap:get(I, FMap, null) || I <- Enumerated].
[null,null,null]
5> FMap1 = fmap:put(1, anton, FMap).
{fmap,[user,email,address],
      '$undef',
      {anton,'$undef','$undef'},
      1}
6> FMap2 = fmap:put(2, "anton@test.com", FMap1).
{fmap,[user,email,address],
      '$undef',
      {anton,"anton@test.com",'$undef'},
      2}
7> FMap3 = fmap:put(3, "Russia, some city", FMap2).
{fmap,[user,email,address],
      '$undef',
      {anton,"anton@test.com","Russia, some city"},
      3}
8> [ fmap:get(I, FMap3, null) || I <- Enumerated].
[anton,"anton@test.com","Russia, some city"]
9> fmap:keys(FMap3).
[{user,1},{email,2},{address,3}]
10> fmap:keys(FMap2).
[{user,1},{email,2}]
11> fmap:values(FMap3).
[anton,"anton@test.com","Russia, some city"]

```

Build
-----

    $ rebar3 compile


Dependencies
-----

You need `rebar3` in your $PATH for building

