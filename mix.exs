defmodule Result.MixProject do
  use Mix.Project

  def project do
    [
      app: :result,
      version: "1.0.0",
      description: "(OK fork) Elegant error/exception handling in Elixir, with result monads.",
      package: package(),
      docs: [
        main: "readme",
        source_url: "https://github.com/ppraisethesun/result",
        extras: ["README.md"]
      ],
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp package do
    [
      name: "res",
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/ppraisethesun/result"},
      source_url: "https://github.com/ppraisethesun/result",
      files: ["lib", "mix.exs", "README*", "LICENSE*"]
    ]
  end

  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end
end
