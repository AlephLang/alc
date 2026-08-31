subdir('allocs')
subdir('containers')
subdir('parser')
subdir('platform')

libalc_src += [
  'libalc/alc.c',
  'libalc/ast.c',
  'libalc/crypto.c',
  'libalc/lexer.c',
  'libalc/module.c',
  'libalc/program.c',
  'libalc/sourcefile.c',
  'libalc/token.c',
]

debug_conf_data = configuration_data()

debug_conf_data.set('__ALC_DEBUG_TOKENS__', get_option('debug_tokens'))
debug_conf_data.set('__ALC_DEBUG_ASTS__', get_option('debug_asts'))

configure_file(
  input: 'debug.h.in',
  output: 'debug.h',
  configuration: debug_conf_data,
)
