defmodule Result do
  @moduledoc false
  @type ok(a) :: {:ok, a}
  @type error(err) :: {:error, err}
  @type t(a, err) :: ok(a) | error(err)

  defmacro __using__(_opts) do
    quote do
      import Result.For
      import Result.Pipe
    end
  end

  @spec map({:ok, a}, (a -> b)) :: {:ok, b} when a: any, b: any
  @spec map({:error, reason}, (any -> any)) :: {:error, reason} when reason: any
  def map({:ok, value}, func) when is_function(func, 1), do: {:ok, func.(value)}
  def map({:error, _} = err, _func), do: err

  @spec map_err({:error, reason_a}, (reason_a -> reason_b)) :: {:error, reason_b}
        when reason_a: any, reason_b: any
  @spec map_err({:ok, a}, (any -> any)) :: {:ok, a} when a: any
  def map_err({:error, reason}, func) when is_function(func, 1), do: {:error, func.(reason)}
  def map_err({:ok, _v} = val, _func), do: val

  @spec tap({:ok, a}, (a -> any)) :: {:ok, a} when a: any
  @spec tap({:error, reason}, (any -> any)) :: {:error, reason} when reason: any
  def tap({:ok, value}, func) when is_function(func, 1) do
    func.(value)
    {:ok, value}
  end

  def tap({:error, _} = err, _), do: err

  @spec tap_err({:ok, a}, (any -> any)) :: {:ok, a} when a: any
  @spec tap_err({:error, reason}, (reason -> any)) :: {:error, reason} when reason: any
  def tap_err({:ok, _} = ok, _), do: ok

  def tap_err({:error, reason} = err, func) when is_function(func, 1) do
    func.(reason)
    err
  end

  @spec flat_map({:ok, a} | {:error, reason}, (a -> {:ok, b} | {:error, reason})) ::
          {:ok, b} | {:error, reason}
        when a: any, b: any, reason: any
  def flat_map({:ok, value}, func) when is_function(func, 1), do: func.(value)
  def flat_map({:error, _} = err, _func), do: err

  @spec fold({:ok, a}, (a -> b), (term -> term)) :: b when a: term, b: term
  @spec fold({:error, a}, (term -> term), (a -> b)) :: b when a: term, b: term
  def fold({:ok, value}, func, _) when is_function(func, 1), do: func.(value)
  def fold({:error, reason}, _, func) when is_function(func, 1), do: func.(reason)

  @spec or_else({:ok, a}, (term -> term)) :: {:ok, a} when a: term
  @spec or_else({:error, a}, (a -> b)) :: b when a: term, b: t(term, term)
  def or_else({:ok, value}, _), do: {:ok, value}
  def or_else({:error, reason}, func) when is_function(func, 1), do: func.(reason)

  @spec unwrap_or_else({:ok, a}, (term -> term)) :: a when a: term
  @spec unwrap_or_else({:error, a}, (a -> b)) :: b when a: term, b: term
  def unwrap_or_else({:ok, a}, _), do: a
  def unwrap_or_else({:error, reason}, func) when is_function(func, 1), do: func.(reason)

  @spec map_all([a], (a -> {:ok, b} | {:error, reason})) :: {:ok, [b]} | {:error, reason}
        when a: any, b: any, reason: any
  def map_all(list, func) when is_function(func, 1) do
    result =
      Enum.reduce_while(list, [], fn value, acc ->
        case func.(value) do
          {:ok, value} ->
            {:cont, [value | acc]}

          {:error, _} = error ->
            {:halt, error}
        end
      end)

    if is_list(result), do: {:ok, Enum.reverse(result)}, else: result
  end

  @spec check({:ok, a}, (a -> boolean), test_failure_reason) ::
          {:ok, a} | {:error, test_failure_reason}
        when a: any, test_failure_reason: any
  @spec check({:error, reason}, (a -> boolean), test_failure_reason) :: {:error, reason}
        when a: any, reason: any, test_failure_reason: any

  def check({:ok, value}, func, reason) when is_function(func, 1) do
    if func.(value) do
      {:ok, value}
    else
      {:error, reason}
    end
  end

  def check({:error, reason}, _func, _reason), do: {:error, reason}

  @spec success?({:ok, a}) :: true when a: any
  @spec success?({:error, reason}) :: false when reason: any
  def success?({:ok, _value}), do: true
  def success?({:error, _reason}), do: false

  @spec failure?({:ok, a}) :: false when a: any
  @spec failure?({:error, reason}) :: true when reason: any
  def failure?({:ok, _value}), do: false
  def failure?({:error, _reason}), do: true

  @doc guard: true
  @spec is_success(term()) :: Macro.t()
  defguard is_success(result)
           when is_tuple(result) and tuple_size(result) === 2 and elem(result, 0) === :ok

  @doc guard: true
  @spec is_failure(term()) :: Macro.t()
  defguard is_failure(result)
           when is_tuple(result) and tuple_size(result) === 2 and elem(result, 0) === :error

  defmacro success(value) do
    quote do
      {:ok, unquote(value)}
    end
  end

  defmacro failure(reason) do
    quote do
      {:error, unquote(reason)}
    end
  end

  def wrap({:ok, _} = value), do: value
  def wrap({:error, _} = err), do: err
  def wrap(other), do: {:ok, other}

  @spec from_nullable(any, any) :: {:ok, any} | {:error, any}
  def from_nullable(value, reason \\ :value_required)
  def from_nullable(nil, reason), do: {:error, reason}
  def from_nullable(value, _reason), do: {:ok, value}

end
