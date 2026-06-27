project('alc', 'c')

cc = meson.get_compiler('c')

flags = [
  '-std=c99', '-Wall', '-Wextra', '-Werror=all', '-fvisibility=hidden',
]

libalc_src = []
libalc_inc = include_directories('include')
libalc_deps = []
libalc_name = 'alc'

subdir('libalc')

buildtype = get_option('buildtype')

if buildtype == 'debug' or buildtype == 'debugoptimized'
  flags += [ '-ggdb', '-D_DEBUG' ]
endif

system = host_machine.system()

if system == 'windows'
  flags += '-D_CRT_SECURE_NO_WARNINGS'
  libalc_name = 'libalc'
elif system == 'linux'
  libalc_deps += cc.find_library('m')
endif

libalc_shared = shared_library(
  libalc_name,
  libalc_src,
  include_directories: [ libalc_inc, include_directories('libalc') ],
  dependencies: libalc_deps,
  c_args: flags,
)

libalc = declare_dependency(
  link_with: libalc_shared,
  include_directories: libalc_inc,
)

src = []
subdir('src')

alc_exe = executable(
  'alc',
  src,
  dependencies: libalc,
  include_directories: include_directories('src'),
  c_args: flags,
)
