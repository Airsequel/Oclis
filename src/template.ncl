{
  name = "{{name}}",
  description = m%"
    {{name}} is an awesome CLI tool
  "%,
  version = "0.0.0",
  options = [
    {
      name = "debug",
      description = "Print debug output",
    },
  ],
  commands = [
    {
      name = "hello",
      description = "Print \"hello\"",
    },
  ],
}
