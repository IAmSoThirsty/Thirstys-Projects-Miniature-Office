defmodule DepartmentFloor.CLI do
  @moduledoc "escript entrypoint referenced by mix.exs"

  def main(_args) do
    IO.puts("DepartmentFloor floor=#{DepartmentFloor.floor_number()} language=#{DepartmentFloor.language()} (prototype)")
  end
end
