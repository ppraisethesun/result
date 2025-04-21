defmodule Result.For do
  defmacro for(do: binding, after: yield_block) do
    {:__block__, _env, bindings} = wrap_code_block(binding)

    safe_yield_block =
      quote location: :keep, generated: true do
        unquote(__MODULE__).wrap(unquote(yield_block))
      end

    expand_bindings(bindings, safe_yield_block)
  end

  defmacro for(do: bind_block, after: yield_block, else: exception_clauses) do
    {:__block__, _env, bindings} = wrap_code_block(bind_block)

    safe_yield_block =
      quote location: :keep do
        unquote(__MODULE__).wrap(unquote(yield_block))
      end

    quote location: :keep, generated: true do
      with {:error, reason} <- unquote(expand_bindings(bindings, safe_yield_block)) do
        case reason do
          unquote(exception_clauses)
        end
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
        rescue
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

  defp wrap_code_block({_, env, _} = expression) do
    {:__block__, env, [expression]}
  end

  defp wrap_code_block(literal) do
    {:__block__, [], [literal]}
  end

  defp expand_bindings([{:<-, env, [left, right]} | rest], yield_block) do
    line = Keyword.get(env, :line)

    normal_cases =
      quote line: line, generated: true do
        {:ok, unquote(left)} ->
          unquote(expand_bindings(rest, yield_block))

        {:error, reason} ->
          {:error, reason}
      end

    warning_case =
      quote line: line, generated: true do
        return ->
          raise %Result.BindError{
            return: return,
            lhs: unquote(Macro.to_string(left)),
            rhs: unquote(Macro.to_string(right))
          }
      end

    quote line: line do
      case unquote(right) do
        unquote(normal_cases ++ warning_case)
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
