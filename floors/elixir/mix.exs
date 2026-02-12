defmodule DepartmentFloor.MixProject do
  use Mix.Project

  def project do
    [
      app: :department_floor,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {DepartmentFloor.Application, []}
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"}
    ]
  end

  defp escript do
    [
      main_module: DepartmentFloor.CLI,
      name: "department_floor"
    ]
  end
end
