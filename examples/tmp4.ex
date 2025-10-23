defmodule Mod1 do
	def fun1(), do: {:ok, "Hello"}
end

f = fn -> Mod1 end
f.()			# valid, result: Mod1
f.().fun1()		# valid, result: {:ok, "Hello"}, scope: *
(f.()).fun1()	# valid, result: {:ok, "Hello"}, scope: *
