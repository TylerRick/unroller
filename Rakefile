require 'rubygems'
gem 'quality_rake_tasks'
require 'quality_rake_tasks'

#---------------------------------------------------------------------------------------------------------------------------------
# Specification and description

module Project
  PrettyName = "Ruby Unroller"
  Name       = "unroller"
  RubyForgeName = "unroller"
  Version    = "1.0.1" 
end

specification = Gem::Specification.new do |s|
  s.name    = Project::Name
  s.summary = "Ruby Code Unroller: A tool for generating human-readable \"execution traces\""
  s.version = Project::Version
  s.author  = 'Tyler Rick'
  s.description = <<-EOF
    A debugging/tracing tool for Ruby. Can be used interactively or non-interactively. While it is enabled, it will watch every Ruby statement and method call that gets executed and will display the source code that is being executed in real time on your screen.

    It is recommended that you also install arika-ruby-termios so that you don't have to press enter after each command.
  EOF
  s.email = "rubyforge.org@tylerrick.com"
  s.homepage = "http://#{Project::Name}.rubyforge.org"
  s.rubyforge_project = Project::Name
  s.platform = Gem::Platform::RUBY
  s.add_dependency("facets", '>=2.4.1')
  s.add_dependency("quality_extensions", '>=1.1.2')
  s.add_dependency("colored")
  #s.add_dependency("arika-ruby-termios")

  # Documentation
  s.has_rdoc = true
  s.extra_rdoc_files = ['Readme']
  s.rdoc_options << '--title' << Project::Name << '--main' << 'Readme' << '--line-numbers'

  # Files
  s.files = FileList['{lib,test,examples}/**/*.rb', 'bin/*', 'Readme'].exclude('ToDo').to_a
  s.test_files = Dir.glob('test/*.rb')
  s.require_path = "lib"
end


#---------------------------------------------------------------------------------------------------------------------------------
# Tests

require 'rake/testtask'

task :default => :test
SharedTasks.normal_test_task

#---------------------------------------------------------------------------------------------------------------------------------
# Documentation

require 'rake/rdoctask'

SharedTasks.rdoc_task do |task|
  task.title = Project::PrettyName
  task.rdoc_files.include()
end

#---------------------------------------------------------------------------------------------------------------------------------
# Packaging

SharedTasks.package_task(specification)
SharedTasks.inc_version(__FILE__)

#---------------------------------------------------------------------------------------------------------------------------------
# Publishing

SharedTasks.publish_task

#---------------------------------------------------------------------------------------------------------------------------------
