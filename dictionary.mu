/#
	Dictionaries are a generic data structure for sorting key->value pairs.
	
	The data format will look like so:
		key1:=value1/|key2:=value2/|key3:=value3/|

	Why the crazy delimiters? A few reasons:
		1) I wanted to make keys and values support as wide a possible set of valid 
		strings as possible.
		2) I wanted it to be visually easy on the eyes to distinguish the key side
		from the value side and to separate the values. From a visual standpoint, the
		ideal structure is:
			key1=value1|key2=value2|key3=value3
		However, that means: values can't have | in them and keys can't have | or = in
		them. I briefly considered this instead:
			key1::value1||key2::value2||key3::value3
		I really liked the visual distinction and thought this was nice and still clear,
		but ran into a problem: what if someone sets a key to be "hello:". Suddenly keys
		can be of any character, but they may not end in certain characters-- those rules
		sorta suck.
		
		I settled on the above as something visually clear, but since its a combination 
		of two different characters there's no stray end character to disallow. We
		instead simply edit out ":=" and "/|" period.
		
	Four more rules, though they're soft-- more conventions.
		1) Values should not have <-- in them unless you are participating in the
		push/pop system (see below).
		
		2) Values should not begin with == unless they are numeric (and there's really
		no reason to unless you are using the push/pop system.
		
		3) Keys that begin with _ should be treated as special purpose. They are not
		returned with *-keys() unless the second argument passed is 1, as they're used
		to mark behavior internal to whatever system is using the dictionaries.
		
		4) Keys that begin with _$ are private to the Core Library.
#/

@@ CLEAN-KEY(str) --> Take str, and remove invalid characters for dict keys
-&API_CLEAN-KEY #_guf=
	lcstr(edit(%0,:=,/|,))

@@ CLEAN-VALUE(str) --> Take str, and remove invalid characters for dict values
-&API_CLEAN-VALUE #_guf=
	edit(%0,/|,)

@@ CLEAN-EVAL(str) --> Take str, and make it safe to evaluate, preserving only %r/%t/%b
-&API_CLEAN-EVAL #_guf=
	edit(
		secure(
			edit(
				translate(%0,p),
				%%r,@RETURN@,
				%%R,@RETURN@,
				%%t,@TAB@,
				%%T,@TAB@,
				%%B,@SPACE@,
				%%b,@SPACE@
			)
		),
		@RETURN@,%%r,
		@TAB@,%%t,
		@SPACE@,%%b
	)

@@ DICT-GET-RAW(data, key) --> get key from data
&API_DICT-GET-RAW #_guf=
	after(
		grab(%0,[clean-key(%1)]:=*,/|),
		:=
	),

@@ DICT-GET(data, key) --> get key from data, processing minimally
-&API_DICT-GET #_guf=
	switch(dict-get-raw(%0,%1),
		==*, 
			[after(#$,==)],
		#$
	)

@@ DICT-EVAL(data, key) --> get key from data, fully evaluating
-&API_DICT-EVAL #_guf=
	switch(dict-get(%0,%1),
		+*,
			u(#_guf/_FN_CALC_DICT_VALUE,#$),
		plus*,
			u(#_guf/_FN_CALC_DICT_VALUE,#$),
		-*,
			u(#_guf/_FN_CALC_DICT_VALUE,#$),
		sub*,
			u(#_guf/_FN_CALC_DICT_VALUE,#$),
		
		before(
			first(#$,<--),
			::)
	)
	
-&_FN_CALC_DICT_VALUE #_guf=
	ladd(
		iter(%0,
			edit(before(%i0,::),sub,-,plus,+),
			<--
		)
	)
	
@@ DICT-DEL(data, key) --> return dict with key removed
-&API_DICT-DEL #_guf=
	remove(%0,
		grab(%0,[clean-key(%1)]:=*,/|),
		/|
	)

@@ DICT-SET(data, key, value) --> returns dict with key set to value
-&API_DICT-SET #_guf=
	setunion(
		dict-del(data,%1),
		[clean-key(%1)]:=[clean-value(%2)],
		/|
	)

@@ DICT-KEYS(data) --> returns a /| delimited list of keys
-&API_DICT-KEYS #_guf=
	iter(%0,before(itext(0),:=),/|,/|)
	
@@ DICT-UPDATE(data1, data2) --> updates data1 with data2. Keys not in data1 are set, while keys that are have their values updated.
-&API-LOCAL_DICT-UPDATE #_guf=
	[setq(N,%2)]
	[iter(%0,
		ifelse(setr(0,
					grab(%qN,
						[before(itext(0),:=)]:=*,/|
					)
				)
			,[before(itext(0),:=)]:=[after(%q0,:=)]
				[setq(N,
					remove(%qN,%q0,/|)
				)],
			itext(0)),
		/|,/|
	)]
	[if(%qN,/|%qN)]
	
@@ ----------------------------------------------------------------------------
@@ Q-Register Dictionaries
@@ ----------------------------------------------------------------------------
@@
@@ These are convienence functions for working with dictionaries stored in a
@@ Q-register. Things like
@@    setq(S,dict-set(%qS,key,value))
@@ are a bit awkward, so we simply do q-set(S,key,value)
@@

@@ Q-GET(register, key)
-&API_Q-GET #_guf=
	dict-get(r(%0),%1)

@@ Q-GET-RAW(register, key)
-&API_Q-GET-RAW #_guf=
	dict-get-raw(r(%0),%1)
	
@@ Q-EVAL(register, key)
-&API_Q-EVAL #_guf=
	dict-eval(r(%0),%1)
	
@@ Q-SET(register, key, value)
-&API_Q-SET #_guf=
	setq(%0,dict-set(r(%0),%1,%2))
	
@@ Q-DEL(register, key)
-&API_Q-DEL #_guf=
	setq(%0,dict-del(r(%0),%1))
	
@@ Q-KEYS(register)
-&API_Q-KEYS #_guf=
	dict-keys(r(%0))
	
@@ Q-UPDATE(register, data)
-&API_Q-UPDATE #_guf=
	setq(%0,dict-update(r(%0),%1))
	
@@ ----------------------------------------------------------------------------
@@ Attribute Dictionaries
@@ ----------------------------------------------------------------------------
@@
@@ Convienence functions for storing dictionaries in attributes.
@@
@@ These are NOT side-effects. To set a value, for example, you must:
@@    &attribute object=[attr-set(object,attribute,key,value)]
@@

@@ ATTR-GET(object, attribute, key)
-&API_ATTR-GET #_guf=
	dict-get(get(%0/%1),%2)

@@ ATTR-GET-RAW(object, attribute, key)
-&API_ATTR-GET-RAW #_guf=
	dict-get-raw(get(%0/%1),%2)
	
@@ ATTR-EVAL(object, attribute, key)
-&API_ATTR-EVAL #_guf=
	dict-eval(get(%0/%1),%2)
	
@@ ATTR-SET(object, attribute, key, value)
-&API_ATTR-SET #_guf=
	dict-set(get(%0/%1),%2,%3)
	
@@ ATTR-DEL(object, attribute, key)
-&API_ATTR-DEL #_guf=
	dict-del(get(%0/%1),%2)
	
@@ ATTR-KEYS(object, attribute)
-&API_ATTR-KEYS #_guf=
	dict-keys(get(%0/%1))
	
@@ ATTR-UPDATE(object, attribute, data)
-&API_ATTR-UPDATE #_guf=
	dict-update(get(%0/%1),%2)
