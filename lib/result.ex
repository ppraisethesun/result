defmodule Result do
  @type ok(a) :: {:ok, a}
  @type error(err) :: {:error, err}
  @type t(a, err) :: ok(a) | error(err)
  @type t :: t(any, any)

  defmacro __using__(_opts) do
    quote do
      import Result.Macro, only: [~>: 2, ~>>: 2]
    end
  end

  @doc """
  Given any value, it makes sure it is a result type
  ## Examples
      iex> Result.of({:ok, 1})
      {:ok, 1}

      iex> Result.of({:error, 2})
      {:error, 2}

      iex> Result.of(3)
      {:ok, 3}
  """
  @spec of(any()) :: t()
  def of({:ok, _} = ok), do: ok
  def of({:error, _} = error), do: error
  def of(:error), do: {:error, :unknown}
  def of(a), do: {:ok, a}

  @doc """
  ## Examples
      iex> Result.map({:ok, 1}, fn a -> a + 1 end)
      {:ok, 2}

      iex> Result.map({:error, :reason}, fn a -> a + 1 end)
      {:error, :reason}
  """
  @spec map({:ok, a}, (a -> b)) :: {:ok, b} when a: any, b: any
  @spec map({:error, reason}, (any -> any)) :: {:error, reason} when reason: any
  def map({:ok, value}, func) when is_function(func, 1), do: {:ok, func.(value)}
  def map({:error, _} = err, _func), do: err

  @doc """
  ## Examples
      iex> Result.map_err({:ok, 1}, fn a -> [details: a] end)
      {:ok, 1}

      iex> Result.map_err({:error, :reason}, fn a -> [details: a] end)
      {:error, details: :reason}
  """
  @spec map_err({:error, reason_a}, (reason_a -> reason_b)) :: {:error, reason_b}
        when reason_a: any, reason_b: any
  @spec map_err({:ok, a}, (any -> any)) :: {:ok, a} when a: any
  def map_err({:error, reason}, func) when is_function(func, 1), do: {:error, func.(reason)}
  def map_err({:ok, _v} = val, _func), do: val

  @doc """
  ## Examples
      iex> Result.chain({:ok, 1}, fn a -> {:ok, a + 1} end)
      {:ok, 2}

      iex> Result.chain({:ok, 1}, fn _a -> {:error, :reason} end)
      {:error, :reason}

      iex> Result.chain({:error, :reason}, fn a -> {:ok, a + 1} end)
      {:error, :reason}
  """
  @spec chain({:ok, a} | {:error, reason}, (a -> {:ok, b} | {:error, reason})) ::
          {:ok, b} | {:error, reason}
        when a: any, b: any, reason: any
  def chain({:ok, value}, func) when is_function(func, 1), do: func.(value)
  def chain({:error, _} = err, _func), do: err

  @doc ~S"""
  ## Examples
      iex> Result.fold(
      ...>   {:ok, 1},
      ...>   fn a -> a + 1 end,
      ...>   fn reason -> "Failed with #{reason}" end
      ...> )
      2

      iex> Result.fold(
      ...> {:error, :reason},
      ...>   fn a -> a + 1 end,
      ...>   fn reason -> "Failed with #{reason}" end
      ...> )
      "Failed with reason"
  """
  @spec fold({:ok, a}, (a -> b), (term -> term)) :: b when a: term, b: term
  @spec fold({:error, a}, (term -> term), (a -> b)) :: b when a: term, b: term
  def fold({:ok, value}, func, _) when is_function(func, 1), do: func.(value)
  def fold({:error, reason}, _, func) when is_function(func, 1), do: func.(reason)

  @spec tap({:ok, a}, (a -> any)) :: {:ok, a} when a: any
  @spec tap({:error, reason}, (any -> any)) :: {:error, reason} when reason: any
  def tap({:ok, value} = ok, func) when is_function(func, 1) do
    func.(value)
    ok
  end

  def tap({:error, _} = err, _), do: err

  @spec tap_err({:ok, a}, (any -> any)) :: {:ok, a} when a: any
  @spec tap_err({:error, reason}, (reason -> any)) :: {:error, reason} when reason: any
  def tap_err({:ok, _} = ok, _), do: ok

  def tap_err({:error, reason} = err, func) when is_function(func, 1) do
    func.(reason)
    err
  end

  @spec unwrap_or_else({:ok, a}, (term -> term)) :: a when a: term
  @spec unwrap_or_else({:error, a}, (a -> b)) :: b when a: term, b: term
  def unwrap_or_else({:ok, a}, _), do: a
  def unwrap_or_else({:error, reason}, func) when is_function(func, 1), do: func.(reason)

  @doc """
  ## Examples
      iex> Result.map_all(
      ...>   [1,2,3],
      ...>   fn a -> {:ok, a + 1} end
      ...> )
      {:ok, [2,3,4]}

      iex> Result.map_all(
      ...>   [1,2,3],
      ...>   fn
      ...>     3 -> {:error, :unexpected}
      ...>     a -> {:ok, a + 1}
      ...>   end
      ...> )
      {:error, :unexpected}
  """
  @spec map_all([a], (a -> {:ok, b} | {:error, reason})) :: {:ok, [b]} | {:error, reason}
        when a: any, b: any, reason: any
  def map_all(list, func) when is_function(func, 1) do
    list
    |> Enum.reduce_while({:ok, []}, fn value, {:ok, acc} ->
      case func.(value) do
        {:ok, value} -> {:cont, {:ok, [value | acc]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
    |> map(&Enum.reverse/1)
  end

  @spec from_nullable(any, any) :: {:ok, any} | {:error, any}
  def from_nullable(value, reason \\ :value_required)
  def from_nullable(nil, reason), do: {:error, reason}
  def from_nullable(value, _reason), do: {:ok, value}

  @doc """
  ## Examples
      iex> require Result
      ...> Result.for do
      ...>   a <- {:ok, 1}
      ...>   b = a + 1
      ...> after
      ...>   b
      ...> end
      {:ok, 2}

      iex> require Result
      ...> Result.for do
      ...>   a <- {:error, :reason}
      ...>   b = a + 1
      ...> after
      ...>   b
      ...> end
      {:error, :reason}

      iex> require Result
      ...> Result.for do
      ...>   a <- {:error, :reason}
      ...>   b = a + 1
      ...> after
      ...>   b
      ...> else
      ...>   :reason -> {:ok, :handled}
      ...> end
      {:ok, :handled}
  """
  defmacro for(do: binding, after: yield_block) do
    {:__block__, _env, bindings} = wrap_code_block(binding)

    safe_yield_block =
      quote location: :keep do
        unquote(__MODULE__).of(unquote(yield_block))
      end

    expand_bindings(bindings, safe_yield_block)
  end

  defmacro for(do: bind_block, after: yield_block, else: exception_clauses) do
    {:__block__, _env, bindings} = wrap_code_block(bind_block)

    safe_yield_block =
      quote location: :keep do
        unquote(__MODULE__).of(unquote(yield_block))
      end

    quote location: :keep, generated: true do
      case unquote(expand_bindings(bindings, safe_yield_block)) do
        {:error, reason} ->
          case reason do
            unquote(exception_clauses)
          end

        value ->
          value
      end
    end
  end

  defmacro for(_) do
    description = """
    Result.for/1 requires `do` and `after` clauses. e.g.

        Result.for do
          a <- safe_div(8, 2)
          b <- safe_div(a, 2)
        after
          a + b
        else
          :zero_division ->
            {:error, :nan}
        end
    """

    raise %SyntaxError{
      file: __CALLER__.file,
      line: __CALLER__.line,
      description: description
    }
  end

  defp wrap_code_block({:__block__, _env, _lines} = block), do: block
  defp wrap_code_block({_, env, _} = expression), do: {:__block__, env, [expression]}
  defp wrap_code_block(literal), do: {:__block__, [], [literal]}

  defp expand_bindings([{:<-, env, [left, right]} | rest], yield_block) do
    line = Keyword.get(env, :line)

    quote line: line do
      case unquote(right) do
        {:ok, unquote(left)} ->
          unquote(expand_bindings(rest, yield_block))

        {:error, reason} ->
          {:error, reason}

        return ->
          raise %Result.BindError{
            return: return,
            lhs: unquote(Macro.to_string(left)),
            rhs: unquote(Macro.to_string(right))
          }
      end
    end
  end

  defp expand_bindings([normal | rest], yield_block) do
    quote do
      unquote(normal)
      unquote(expand_bindings(rest, yield_block))
    end
  end

  defp expand_bindings([], yield_block) do
    yield_block
  end
end
