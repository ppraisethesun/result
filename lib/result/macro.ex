defmodule Result.Macro do
  @doc """
  ## Examples
      iex> use Result
      ...> {:ok, 1}
      ...> ~> fn a -> a + 1 end
      {:ok, 2}

      iex> use Result
      ...> fun = fn a -> a + 1 end
      ...> {:ok, 1}
      ...> ~> fun.()
      {:ok, 2}

      iex> use Result
      ...> {:ok, 1}
      ...> ~> (& &1 + 1)
      {:ok, 2}

      iex> use Result
      ...> {:error, :reason}
      ...> ~> fn a -> a + 1 end
      {:error, :reason}
  """

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
