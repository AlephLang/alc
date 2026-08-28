subdir('shared')

system = host_machine.system()
if system == 'windows'
  subdir('windows')
else
  # Assume that other systems do not add more unnecessary pain in the ass and are POSIX compliant.
  subdir('posix')
endif
