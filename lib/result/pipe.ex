defmodule Result.Pipe do
  defmacro lhs ~> ({call, _, _} = anon) when call in [:fn, :&] do
    quote do
      Result.map(unquote(lhs), unquote(anon))
    end
  end

  defmacro lhs ~> {call, line, args} do
    value = quote do: value
    args = [value | args || []]

    quote do
      Result.map(unquote(lhs), fn unquote(value) -> unquote({call, line, args}) end)
    end
  end

  defmacro lhs ~>> ({call, _, _} = anon) when call in [:fn, :&] do
    quote do
      Result.flat_map(unquote(lhs), unquote(anon))
    end
  end

  defmacro lhs ~>> {call, line, args} do
    value = quote do: value
    args = [value | args || []]

    quote do
      Result.flat_map(unquote(lhs), fn unquote(value) -> unquote({call, line, args}) end)
    end
  end
end
