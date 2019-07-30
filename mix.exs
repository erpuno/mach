defmodule MACH.Mixfile do
  use Mix.Project
  def deps, do: [ {:ex_doc, ">= 0.0.0", only: :dev}]
  def application, do: [mod: {:mach, []}, applications: []]
  def project do
    [ app: :mach,
      version: "4.7.0",
      description: "MACH REST Walker",
      package: package(),
      deps: deps()]
  end
  def package do
    [ files: ~w(doc include src mix.exs rebar.config LICENSE),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/synrc/mach"}]
  end
end
