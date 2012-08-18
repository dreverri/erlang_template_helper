#!/usr/bin/env rake
require "bundler/gem_tasks"

require 'rake/testtask'
 
Rake::TestTask.new do |t|
  t.libs << 'lib/erlang_template_helper'
  t.test_files = FileList['test/lib/erlang_template_helper/*_test.rb']
  t.verbose = true
end
 
task :default => :test

desc "Open an irb session preloaded with this library"
task :console do
  sh "irb -I lib -r erlang_template_helper.rb"
end
