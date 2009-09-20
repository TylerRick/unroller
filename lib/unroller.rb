#require 'rubygems'
#gem 'facets'
require 'facets'
require 'facets/methodspace'
require 'facets/kernel/populate'
require 'facets/kernel/returning'
#require 'facets/kernel/singleton_class'
#require 'facets/symbol/to_proc'
#require 'facets/string/bracket'
#require 'facets/string/index_all'
#require 'facets/hash/reverse_merge'

#gem 'quality_extensions'
require 'quality_extensions/object/send_if_not_nil'
require 'quality_extensions/kernel/trap_chain'
require 'quality_extensions/kernel/capture_output'
require 'quality_extensions/string/with_knowledge_of_color'
require 'quality_extensions/exception/inspect_with_backtrace'
require 'quality_extensions/regexp/join'
require 'quality_extensions/symbol/match'
require 'quality_extensions/module/alias_method_chain'
require 'quality_extensions/module/malias_method_chain'
require 'quality_extensions/module/attribute_accessors'
require 'quality_extensions/enumerable/select_until'
require 'quality_extensions/module/bool_attr_accessor'
require 'quality_extensions/object/pp_s'

#gem 'colored'
require 'colored'
#gem 'extensions'
#require 'extensions/object'

require 'English'
require 'pp'
require 'stringio'

# To disable color, uncomment this:
#class String
#  def colorize(string, options = {})
#    string
#  end
#end

begin
  #gem 'termios' # or arika-ruby-termios, whose latest vesion is 0.9.6 (the native extension for which installs for me with Ruby 1.9.1) newer than ruby-termios's latest 0.9.4 (which won't install for me)
  require 'termios'
  begin
    # Set up termios so that it returns immediately when you press a key.
    # (http://blog.rezra.com/articles/2005/12/05/single-character-input)
    t = Termios.tcgetattr(STDIN)
    save_terminal_attributes = t.dup
    t.lflag &= ~Termios::ICANON
    Termios.tcsetattr(STDIN, 0, t)

    # Set terminal_attributes back to how we found them...
    at_exit { Termios.tcsetattr(STDIN, 0, save_terminal_attributes) }
  rescue RuntimeError => exception    # Necessary for automated testing.
    if exception.message =~ /can't get terminal parameters/
      puts 'Warning: Terminal not found.'
      $interactive = false
    elsif exception.message =~ /Inappropriate ioctl for device/
      puts "Warning: #{exception.inspect}"
      # This error happens when a Rails app is started with script/server -d
      # By rescuing this error, we should be able to tail log/mongrel.log to see the output.
    else
      raise
    end
  end
  $termios_loaded = true
rescue Gem::LoadError
  $termios_loaded = false
end

class IO
  # Gets a single character, as a string.
  # Adjusts for the different behavior of getc if we are using termios to get it to return immediately when you press a single key
  # or if they are not using that behavior and thus have to press Enter after their single key.
  def getch
    response = getc
    if !$termios_loaded
      next_char = getc
      new_line_characters_expected = ["\n"]
      #new_line_characters_expected = ["\n", "\r"] if windows?
      if next_char.chr.in?(new_line_characters_expected)
        # Eat the newline character
      else
        # Don't eat it
        # (This case is necessary, for escape sequences, for example, where they press only one key, but it produces multiple characters.)
        $stdin.ungetc(next_char)
      end
    end
    response.chr
  end
end

class String
  # Makes the first character bold and underlined. Makes the whole string of the given color.
  # :todo: Move out to extensions/console/menu_item
  def menu_item(color = :white, letter = self[0..0], which_occurence = 0)
    index = index_all(/#{letter}/)[which_occurence]
    raise "Could not find a #{which_occurence}th occurence of '#{letter}' in string '#{self}'" if index.nil?
    before = self[0..index-1].send(color) unless index == 0
    middle = self[index..index].send(color).bold.underline
    after  = self[index+1..-1].send(color)
    before.to_s + middle + after
  end
end
def confirm(question, options = ['Yes', 'No'])
  print question + " " +
    "Yes".menu_item(:red) + ", " +
    "No".menu_item(:green) + 
    " > "
  response = ''
  # Currently allow user to press Enter to accept the default.
  response = $stdin.getc.chr.downcase while !['y', 'n', "\n"].include?(begin response.downcase!; response end)
  response
end

class Unroller
  #-------------------------------------------------------------------------------------------------
  # Helper classes

  class Variables
    def initialize(which, binding)
      @variables = eval("#{which}_variables", binding).map { |variable|
        value = eval(variable, binding)
        [variable, value]
      }
    end
    def to_s
      #@variables.inspect
      ret = @variables.map do |variable|
        name, value = *variable
        "#{name} = #{value.inspect}"
      end.join('; ').bracket('   (', ')')
      ret[0..70] + '...'  # Maybe truncating it could be optional in the future, but for now it's just too cluttered
    end
    def verbose_to_s
      @variables.map do |variable|
        name, value = *variable
        "#{name} = " +
          value.pp_s.gsub(/^/, '  ')
      end.join("\n")
    end
    def any?
      !@variables.empty?
    end
  end
  @@instance = nil
  @@quiting = false

  Call = Struct.new(:file, :line_num, :klass, :name, :full_name)
  # AKA stack frame?

  class ClassExclusion
    bool_attr_reader :recursive
    attr_reader :regexp
    def initialize(klass, *flags)
      raise ArgumentError if !(Module === klass || String === klass || Symbol === klass || Regexp === klass)
      klass = klass.name if Module === klass
      @regexp =
        Regexp === klass ?
          klass :
          /^#{klass.to_s}$/    # (Or should we escape it?)
      @recursive = true if flags.include?(:recursive)
    end
  end

  # Helper classes
  #-------------------------------------------------------------------------------------------------



  def self.debug(options = {}, &block)
    options.reverse_merge! :interactive => true, :display_style => :show_entire_method_body
    self.trace options, &block
  end

  def self.trace(options = {}, &block)
    if @@instance and @@instance.tracing
      # In case they try to turn tracing on when it's already on... Assume it was an accident and don't do anything.
      #puts "@@instance.tracing=#{@@instance.tracing}"
      #return if @@instance and @@instance.tracing       
      #yield if block_given?
    else
      self.display_style = options.delete(:display_style) if options.has_key?(:display_style)
      @@instance = Unroller.new(options)
    end
    @@instance.trace &block
  end

  attr_accessor :depth
  attr_reader   :tracing

  mattr_accessor :display_style

  def initialize(options = {})
    # Defaults
    @@display_style ||= :show_entire_method_body
    @condition = Proc.new { true }  # Only trace if this condition is true. Useful if the place where you put your trace {} statement gets called a lot and you only want it to actually trace for some of those calls.
    @initial_depth = 1              # ("Call stack") depth to start at. Actually, you'll probably want this set considerably lower than the current call stack depth, so that the indentation isn't way off the screen.
    @max_lines = nil                # Stop tracing (permanently) after we have produced @max_lines lines of output. If you don't know where to place the trace(false) and you just want it to stop on its own after so many lines, you could use this...
    @max_depth = nil                # Don't trace anything when the depth is greater than this threshold. (This is *relative* to the starting depth, so whatever level you start at is considered depth "1".)
    @line_matches = nil             # The source code for that line matches this regular expression
    @presets = []
    @file_match = /./
    @exclude_classes = []
    @include_classes = []           # These will override classes that have been excluded via exclude_classes. So if you both exclude and include a class, it will be included.
    @exclude_methods = []
    @include_methods = []
    @show_args   = true
    @show_locals = false
    @show_filename_and_line_numbers = true
    @include_c_calls = false        # Might be useful if you're debugging your own C extension. Otherwise, we probably don't care about C-calls because we can't see the source for them anyway...
    @strip_comments = true          # :todo:
    @use_full_path = false          # :todo:
    @screen_width = 150
    @column_widths = [70]
    @indent_step =       ' ' + '|'.magenta + ' '
    @column_separator = '  ' + '|'.yellow.bold + ' '
    @always_show_raise_events = false
    @show_file_load_errors = false
    @interactive = false   # Set to true to make it more like an interactive debugger.
    @show_menu = true      # Set to false if you don't need the hints.
      # (In the future, might add "break out of this call" option to stop watching anything until we return from the current method... etc.)
    instance_variables.each do |v|
      self.class.class_eval do
        attr_accessor v.to_s.gsub!(/^@/, '')
      end
    end

    # "Presets"
    # Experimental -- subject to change a lot before it's finalized
    options[:presets]       = options.delete(:only)            if options.has_key?(:only)
    options[:presets]       = options.delete(:debugging)       if options.has_key?(:debugging)
    options[:presets]       = options.delete(:preset)          if options.has_key?(:preset)
    options[:presets] = [options[:presets]] unless options[:presets].is_a?(Array)
    [:rails, :dependencies].each do |preset|
      if options.has_key?(preset) || options[:presets].include?(preset)
        options.delete(preset)
        case preset
        when :dependencies    # Debugging ActiveSupport::Dependencies
          @exclude_classes.concat [
            /Gem/
          ].map {|e| ClassExclusion.new(e) }
        when :rails
          @exclude_classes.concat [
            /Benchmark/,
            /Gem/,
            /Dependencies/,
            /Logger/,
            /MonitorMixin/,
            /Set/,
            /HashWithIndifferentAccess/,
            /ERB/,
            /ActiveRecord/,
            /SQLite3/,
            /Class/,
            /ActiveSupport/,
            /ActiveSupport::Deprecation/,
            /Pathname/,
            /Object/,
            /Symbol/,
            /Kernel/,
            /Inflector/,
            /Webrick/
          ].map {|e| ClassExclusion.new(e) }
        end
      end
    end

    #-----------------------------------------------------------------------------------------------
    # Options

    # Aliases
    options[:max_lines]       = options.delete(:head)       if options.has_key?(:head)
    options[:condition]       = options.delete(:if)         if options.has_key?(:if)
    options[:initial_depth]   = options.delete(:depth)      if options.has_key?(:depth)
    options[:initial_depth]   = caller(0).size              if options[:initial_depth] == :use_call_stack_depth
    options[:file_match]      = options.delete(:file)       if options.has_key?(:file)
    options[:file_match]      = options.delete(:path)       if options.has_key?(:path)
    options[:file_match]      = options.delete(:path_match) if options.has_key?(:path_match)
    options[:dir_match]       = options.delete(:dir)        if options.has_key?(:dir)
    options[:dir_match]       = options.delete(:dir_match)  if options.has_key?(:dir_match)

    if (a = options.delete(:dir_match))
      unless a.is_a?(Regexp)
        if a =~ /.*\.rb/
          # They probably passed in __FILE__ and wanted us to File.expand_path(File.dirname()) it for them (and who can blame them? that's a lot of junk to type!!)
          a = File.expand_path(File.dirname(a))
        end
        a = /^#{Regexp.escape(a)}/ # Must start with that entire directory path
      end
      options[:file_match] = a
    end
    if (a = options.delete(:file_match))
      # Coerce it into a Regexp
      unless a.is_a?(Regexp)
        a = /#{Regexp.escape(a)}/ 
      end
      options[:file_match] = a
    end

    if options.has_key?(:exclude_classes)
      # Coerce it into an array of ClassExclusions
      a = options.delete(:exclude_classes)
      a = [a] unless a.is_a?(Array)
      a.map! {|e| e = ClassExclusion.new(e) unless e.is_a?(ClassExclusion); e }
      @exclude_classes.concat a
    end
    if options.has_key?(:include_classes)
      # :todo:
    end
    if options.has_key?(:exclude_methods)
      # Coerce it into an array of Regexp's
      a = options.delete(:exclude_methods)
      a = [a] unless a.is_a?(Array)
      a.map! {|e| e = /^#{e}$/ unless e.is_a?(Regexp); e }
      @exclude_methods.concat a
    end
    options[:line_matches]       = options.delete(:line_matches)       if options.has_key?(:line_matches)
    populate(options)

    #-----------------------------------------------------------------------------------------------
    # Private
    @call_stack = []        # Used to keep track of what method we're currently in so that when we hit a 'return' event we can display something useful.
                            # This is useful for two reasons:
                            # 1. Sometimes (and I don't know why), the code that gets shown for a 'return' event doesn't even look
                            #    like it has anything to do with a return... Having the call stack lets us intelligently say 'returning from ...'
                            # 2. If we've been stuck in this method for a long time and we're really deep, the user has probably forgotten by now which method we are returning from
                            #    (the filename may give some clue, but not enough), so this is likely to be a welcome reminder a lot of the time.
                            # Also using it to store line numbers, so that we can show the entire method definition each time we process a line,
                            # rather than just the current line itself.
                            # Its members are of type Call
    @internal_depth = 0     # This is the "true" depth. It is incremented/decremented for *every* call/return, even those that we're not displaying. It is necessary for the implementation of "silent_until_return_to_this_depth", to detect when we get back to interesting land.
    @depth = @initial_depth # This is the user-friendly depth. It only counts calls/returns that we *display*; it does not change when we enter into a call that we're not displaying (a "hidden" call).
    @output_line = ''
    @column_counter = 0
    @tracing = false
    @files = {}
    @lines_output = 0
    @silent_until_return_to_this_depth = nil
  end

  def self.exclude(*args, &block)
    @@instance.exclude(*args, &block) unless @@instance.nil?
  end
  def self.suspend(*args, &block)
    @@instance.exclude(*args, &block) unless @@instance.nil?
  end
  def exclude(&block)
    old_tracing = @tracing
    (trace_off; puts 'Suspending tracing')
    yield
    (trace; puts 'Resuming tracing') if old_tracing
  end

  def trace(&block)
  catch :quit do
    throw :quit if @@quiting
    if @tracing
      yield if block_given?
      # No need to call set_trace_func again; we're already tracing
      return
    end

    begin
      @tracing = true


      if @condition.call

        trap_chain("INT") do
          puts
          puts 'Exiting trace...'
          set_trace_func(nil)
          @@quiting = true
          throw :quit
        end









        # (This is the meat of the library right here, so let's set it off with at least 5 blank lines.)
        set_trace_func( lambda do |event, file, line, id, binding, klass|
          return if @@quiting
          begin # begin/rescue block
            @event, @file, @line, @id, @binding, @klass =
              event, file, line, id, binding, klass
            line_num = line
            current_call = Call.new(file, line, klass, id, fully_qualified_method)

            # Sometimes klass is false and id is nil. Not sure why, but that's the way it is.
            #printf "- (event=%8s) (klass=%10s) (id=%10s) (%s:%-2d)\n", event, klass, id, file, line #if klass.to_s == 'false'
            #puts 'false!!!!!!!'+klass.inspect if klass.to_s == 'false'

            return if ['c-call', 'c-return'].include?(event) unless include_c_calls
            #(puts 'exclude') if @silent_until_return_to_this_depth unless event == 'return' # Until we hit a return and can break out of this uninteresting call, we don't want to do *anything*.
            #return if uninteresting_class?(klass.to_s) unless (klass == false)

            if @only_makes_sense_if_next_event_is_call
              if event == 'call'
                @only_makes_sense_if_next_event_is_call = nil
              else
                # Cancel @silent_until_return_to_this_depth because it didn't make sense / wasn't necessary. They could have 
                # ("should have") simply done a 'step into', because unless it's a 'call', there's nothing to step over anyway...
                # Not only is it unnecessary, but it would cause confusing behavior unless we cancelled this. As in, it
                # wouldn't show *any* tracing for the remainder of the method, because it's kind of looking for a "return"...
                #puts "Cancelling @silent_until_return_to_this_depth..."
                @silent_until_return_to_this_depth = nil
              end
            end

            if too_far?
              puts "We've read #{@max_lines} (@max_lines) lines now. Turning off tracing..."
              trace_off
              return
            end

            case @@display_style

            # To do: remove massive duplication with the other (:concise) display style
            when :show_entire_method_body

              case event


                #zzz
                when 'call'
                  unless skip_line?
                    depth_debug = '' #" (internal_depth about to be #{@internal_depth+1})"
                    column sprintf(' ' + '\\'.cyan + ' calling'.cyan + ' ' + '%s'.underline.cyan + depth_debug, 
                                   fully_qualified_method), @column_widths[0]
                    newline

                    #puts
                    #header_before_code_for_entire_method(file, line_num)
                    #do_show_locals if show_args
                    #ret = code_for_entire_method(file, line, klass, id, line, 0)
                    #puts ret unless ret.nil?
                    #puts

                    @lines_output += 1


                    @depth += 1
                  end

                  @call_stack.push current_call
                  @internal_depth += 1
                  #puts "(@internal_depth+=1 ... #{@internal_depth})"


                when 'class'
                when 'end'
                when 'line'
                  unless skip_line?
                    # Show the state of the locals *before* executing the current line. (I might add the option to show it after instead/as well, but I don't think that would be easy...)

                    inside_of = @call_stack.last
                    #puts "inside_of=#{inside_of.inspect}"
                    if inside_of
                      unless @last_call == current_call  # Without this, I was seeing two consecutive events for the exact same line. This seems to be necessary because 'if foo()' is treated as two 'line' events: one for 'foo()' and one for 'if' (?)...
                        puts
                        header_before_code_for_entire_method(file, line_num)
                        do_show_locals if true #show_locals
                        ret = code_for_entire_method(inside_of.file, inside_of.line_num, @klass, @id, line, -1)
                        puts ret unless ret.nil?
                        puts
                      end
                    else
                      column pretty_code_for(file, line, ' ', :bold), @column_widths[0]
                      file_column file, line
                      newline
                    end

                    # Interactive debugger!
                    response = nil
                    if @interactive && !(@last_call == current_call)
                      #(print '(o = Step out of | s = Skip = Step over | default = Step into > '; response = $stdin.gets) if @interactive

                      while response.nil? or !response.in? ['i',' ',"\e[C","\e[19~", 'v',"\e[B","\e[20~", 'u',"\e[D", 'r', "\n", 'q'] do
                        print "Debugger (" +
                          "Step into (F8/Right/Space)".menu_item(:green, 'i') + ' | ' + 
                          "Step over (F9/Down/Enter)".menu_item(:cyan, 'v') + ' | ' +
                          "Step out (Left)".menu_item(:red, 'u') + ' | ' +
                          "show Locals".menu_item(:yellow, 'l') + ' | ' + 
                          "Run".menu_item(:blue) + ' | ' + 
                          "Quit".menu_item(:magenta) + 
                          ') > '
                        $stdout.flush

                        response = $stdin.getch.downcase

                        # Escape sequence such as the up arrow key ("\e[A")
                        if response == "\e"
                          response << (next_char = $stdin.getch)
                          if next_char == '['
                            response << (next_char = $stdin.getch)
                            if next_char.in? ['1', '2']
                              response << (next_char = $stdin.getch)
                              response << (next_char = $stdin.getch)
                            end
                          end
                        end

                        puts unless response == "\n"

                        case response
                        when 'l'
                          do_show_locals_verbosely
                          response = nil
                        end
                      end
                    end

                    if response
                      case response
                      when 'i', ' ', "\e[C", "\e[19~"  # (Right, F8)
                        # keep right on tracing...
                      when 'v', "\n", "\e[B", "\e[20~"  # (Down, F9) Step over = Ignore anything with a greater depth.
                        @only_makes_sense_if_next_event_is_call = true
                        @silent_until_return_to_this_depth = @internal_depth
                      when 'u', "\e[D" # (Left) Step out
                        @silent_until_return_to_this_depth = @internal_depth - 1
                        #puts "Setting @silent_until_return_to_this_depth = #{@silent_until_return_to_this_depth}"
                      when 'r' # Run
                        @interactive = false
                      when 'q'
                        @@quiting = true
                        throw :quit
                      else
                        # we shouldn't get here
                      end
                    end

                    @last_call = current_call
                    @lines_output += 1
                  end # unless skip_line?


                when 'return'

                  @internal_depth -= 1
                  #puts "(@internal_depth-=1 ... #{@internal_depth})"

                  # Did we just get out of an uninteresting call?? Are we back in interesting land again??
                  if  @silent_until_return_to_this_depth and 
                      @silent_until_return_to_this_depth == @internal_depth
                    #puts "Yay, we're back in interesting land! (@internal_depth = #{@internal_depth})"
                    @silent_until_return_to_this_depth = nil
                  end


                  unless skip_line?
                    puts "Warning: @depth < 0. You may wish to call trace with a :depth => depth value greater than #{@initial_depth}" if @depth-1 < 0
                    @depth -= 1 unless @depth == 0
                    #puts "-- Decreased depth to #{depth}"
                    returning_from = @call_stack.last

                    depth_debug = '' #" (internal_depth was #{@internal_depth+1})"
                    column sprintf(' ' + '/'.cyan + ' returning from'.cyan + ' ' + '%s'.cyan + depth_debug,
                                   returning_from && returning_from.full_name), @column_widths[0]
                    newline

                    @lines_output += 1
                  end

                  @call_stack.pop


                when 'raise'
                  if !skip_line? or @always_show_raise_events
                    # We probably always want to see these (?)... Never skip displaying them, even if we are "too deep".
                    column "Raising an error (#{$!}) from #{klass}".red.bold, @column_widths[0]
                    newline

                    column pretty_code_for(file, line, ' ').red, @column_widths[0]
                    file_column file, line
                    newline
                  end

                else
                  column sprintf("- (%8s) %10s %10s (%s:%-2d)", event, klass, id, file, line)
                  newline
              end # case event

            # End when :show_entire_method_body

            when :concise
              case event


                when 'call'
                  unless skip_line?
                    # :todo: use # instead of :: if klass.constantize.instance_methods.include?(id)
                    column sprintf(' ' + '+'.cyan + ' calling'.cyan + ' ' + '%s'.underline.cyan, fully_qualified_method), @column_widths[0]
                    newline

                    column pretty_code_for(file, line, '/'.magenta, :green), @column_widths[0]
                    file_column file, line
                    newline

                    @lines_output += 1

                    @call_stack.push Call.new(file, line, klass, id, fully_qualified_method)

                    @depth += 1
                    #puts "++ Increased depth to #{depth}"

                    # The locals at this point will be simply be the arguments that were passed in to this method.
                    do_show_locals if show_args
                  end
                  @internal_depth += 1


                when 'class'
                when 'end'
                when 'line'
                  unless skip_line?
                    # Show the state of the locals *before* executing the current line. (I might add the option to show it after instead/as well, but I don't think that would be easy...)
                    do_show_locals if show_locals

                    column pretty_code_for(file, line, ' ', :bold), @column_widths[0]
                    file_column file, line
                    newline

                    @lines_output += 1
                  end



                when 'return'

                  @internal_depth -= 1
                  unless skip_line?
                    puts "Warning: @depth < 0. You may wish to call trace with a :depth => depth value greater than #{@initial_depth}" if @depth-1 < 0
                    @depth -= 1 unless @depth == 0
                    #puts "-- Decreased depth to #{depth}"
                    returning_from = @call_stack.last

                    code = pretty_code_for(file, line, '\\'.magenta, :green, suffix = " (returning from #{returning_from && returning_from.full_name})".green)
                    code = pretty_code_for(file, line, '\\'.magenta + " (returning from #{returning_from && returning_from.full_name})".green, :green) unless code =~ /return|end/
                      # I've seen some really weird statements listed as "return" statements.
                      # I'm not really sure *why* it thinks these are returns, but let's at least identify those lines for the user. Examples:
                      # * must_be_open!
                      # * @db = db
                      # * stmt = @statement_factory.new( self, sql )
                      # I think some of the time this happens it might be because people pass the wrong line number to eval (__LINE__ instead of __LINE__ + 1, for example), so the line number is just not accurate.
                      # But I don't know if that explains all such cases or not...
                    column code, @column_widths[0]
                    file_column file, line
                    newline

                    @lines_output += 1
                  end

                  # Did we just get out of an uninteresting call?? Are we back in interesting land again??
                  if  @silent_until_return_to_this_depth and 
                      @silent_until_return_to_this_depth == @internal_depth
                    puts "Yay, we're back in interesting land!"
                    @silent_until_return_to_this_depth = nil
                  end
                  @call_stack.pop


                when 'raise'
                  if !skip_line? or @always_show_raise_events
                    # We probably always want to see these (?)... Never skip displaying them, even if we are "too deep".
                    column "Raising an error (#{$!}) from #{klass}".red.bold, @column_widths[0]
                    newline

                    column pretty_code_for(file, line, ' ').red, @column_widths[0]
                    file_column file, line
                    newline
                  end

                else
                  column sprintf("- (%8s) %10s %10s (%s:%-2d)", event, klass, id, file, line)
                  newline
              end # case event

            # End default display style
  
            end # case @@display_style



          rescue Exception => exception
            puts exception.inspect
            raise
          end # begin/rescue block
        end) # set_trace_func








      end # if @condition.call

      if block_given?
        yield
      end

    ensure
      trace_off if block_given?
    end # rescue/ensure block
  end
  end # def trace(&block)

  class << self
    alias_method :trace_on, :trace
  end

  def self.trace_off
    if @@instance and @@instance.tracing
      @@instance.trace_off
    end
  end
  def trace_off
    @tracing = false
    set_trace_func(nil)
  end

  #----------------------------------------------------------
  def self.watch_for_added_methods(mod, filter = //, &block)
    mod.singleton_class.instance_eval do
      define_method :method_added_with_watch_for_added_methods do |name, *args|
        if name.to_s =~ filter
          puts "Method '#{name}' was defined at #{caller[0]}"
        end
      end
      alias_method_chain :method_added, :watch_for_added_methods, :create_target => true
    end

    yield if block_given?

#    mod.class.instance_eval do
#      alias_method :method_added, :method_added_without_watch_for_added_methods
#    end
  end



protected
  #----------------------------------------------------------
  # Helpers

  def fully_qualified_method
    "#{@klass}::#{@id}"
  end
  def do_show_locals
    variables = Variables.new(:local, @binding)
    # puts "In do_show_locals at depth #{depth}; event = #{@event}"
    if variables.any?
      column variables.to_s, nil, :allow, :yellow
      newline
    end
  end
  def skip_line?
    @silent_until_return_to_this_depth or 
      !calling_method_in_an_interesting_class?(@klass.to_s) or 
      !calling_interesting_method?(@id.to_s) or 
      too_deep? or 
      !calling_interesting_line?
  end
  def too_deep?
    # The + 1 is because we want it to be 1-based, for humans: if they're still at the initial depth (@depth - @initial_depth == 0), we want it treated as "depth 1".
    @max_depth and (@depth - @initial_depth + 1 > @max_depth)
  end
  def too_far?
    @max_lines and (@lines_output > @max_lines)
  end
  def calling_method_in_an_interesting_class?(class_name)
    !( @exclude_classes + [ClassExclusion.new(self.class.name)] ).any? do |class_exclusion|
      returning(class_name =~ class_exclusion.regexp) do |is_uninteresting|
        if is_uninteresting && class_exclusion.recursive? && @silent_until_return_to_this_depth.nil?
          puts "Turning tracing off until we get back to internal_depth #{@internal_depth} again because we're calling uninteresting #{class_name}:#{@id}"
          @silent_until_return_to_this_depth = @internal_depth
        end
      end
    end
  end
  def calling_interesting_method?(name)
    !( @exclude_methods ).any? do |regexp|
      returning(name =~ regexp) do |is_uninteresting|
        if is_uninteresting && (recursive = implemented = false) && @silent_until_return_to_this_depth.nil?
          puts "Turning tracing off until we get back to internal_depth #{@internal_depth} again because we're calling uninteresting #{@klass}:#{@id}"
          @silent_until_return_to_this_depth = @internal_depth
        end
      end
    end
  end
  def calling_interesting_line?
    path = File.expand_path(@file)  # rescue @file
    #puts "Checking #{path} !~ #{@file_match}"
    return false if path !~ @file_match
    return true if @line_matches.nil?   # No filter to apply
    line = code_for(@file, @line) or return false
    (line =~ @line_matches)
  end

  #----------------------------------------------------------
  # Formatting stuff.

  def indent(indent_adjustment = 0)
    @indent_step * (@depth + indent_adjustment)
  end
  # The same thing, only just using whitespace.
  def plain_indent(indent_adjustment = 0)
    (' '*@indent_step.length_without_color) * [(@depth + indent_adjustment), 0].max
  end

  def remaining_width
    @screen_width - @output_line.length_without_color
  end

  # +width+ is the minimum width for this column. It's also a maximum if +column_overflow+ is :chop_left or :chop_right
  # +color+ is only needed if you plan on doing some chopping, because if you apply the color *before* the chopping, the color code might get chopped off.
  def column(string, width = nil, column_overflow = :allow, color = nil)
    raise ArgumentError if ![:allow, :chop_left, :chop_right].include?(column_overflow)
    raise ArgumentError if width and !(width == :remainder or width.is_a?(Fixnum))

    if width == :remainder
      width = remaining_width()
    end

    if @column_counter == 0
      @output_line << indent
      width -= indent.length_without_color if width
    else
      @output_line << @column_separator      # So the columns won't be squashed up against each other
    end

    if width
      # Handles maximum width
      max_for_column = width
      max_for_screen = remaining_width
      if column_overflow =~ /chop_/   # The column only *has* a max with if column_overflow is chop_ (not if it's allow, obviously)
        #puts "width = #{width}"
        string = string.code_unroller.make_it_fit(max_for_column, column_overflow)
      else
        string = string.code_unroller.make_it_fit(max_for_screen)
      end

      string = string.ljust_with_color(width)    # Handles minimum width
    end

    @output_line << string.send_if_not_nil(color)

    @column_counter += 1
  end # def column

  def newline
    unless @output_line.strip.length_without_color == 0 or @output_line == @last_line_printed
      Kernel.print @output_line
      Kernel.puts
      @last_line_printed = @output_line
    end

    @output_line = ''
    @column_counter = 0
  end

  def file_column(file, line)
    if show_filename_and_line_numbers
      column "#{file}:#{line}", :remainder, :chop_left, :magenta
    end
  end

  #----------------------------------------------------------

  def code_for_file(file)
    return nil if file ==  '(eval)'   # Can't really read the source from the 'eval' file, unfortunately!
    begin
      lines = File.readlines(file)
      lines.unshift ''                # Adjust for the fact that readlines line is 0-based, but the line numbers we'll be dealing with are 1-based.
      @files[file] ||= lines
      @files[file]
    rescue Errno::ENOENT
      $stderr.puts( message = "Error: Could not open #{file}" ) if @show_file_load_errors
      message
    rescue Exception => exception
      puts "Error in code_for_file(#{file}):"
      puts exception.inspect
    end
  end

  def code_for(file, line_num)
    code_for_file = code_for_file(file)
    return nil if code_for_file.nil?

    begin
      line_num = [code_for_file.size - 1, line_num].min    # :todo: We should probably just return an error if it's out of range, rather than doing this min junk...
      line = code_for_file[line_num].strip
    rescue Exception => exception
      puts "Error while getting code for #{file}:#{line_num}:"
      puts exception.inspect
    end
  end
  def pretty_code_for(file, line_num, prefix, color = nil, suffix = '')
    if file == '(eval)'
      return ' ' + prefix + ' ' + file.send_if_not_nil(color) + ''
    end

    ' ' + prefix + ' ' + 
      code_for(file, line_num).to_s.send_if_not_nil(color) +
      suffix
  end

  #----------------------------------------------------------

  def code_for_entire_method(file, line_num, klass, method, line_to_highlight = nil, indent_adjustment = 0)
    code_for_file = code_for_file(file)
    return nil if code_for_file.nil?

    output = ''
    begin

      line_num = [code_for_file.size - 1, line_num].min    # :todo: We should probably just return an error if it's out of range, rather than doing this min junk...
      first_line = code_for_file[line_num]

      if first_line =~ /^(\s+)\S?.*def/
        # About the regexp:
        #   It would just be as simple as /^(\s+)def/ except that we want to allow for the (admittedly rare) case where they have it
        #   *indented* correctly, but they have other junk at the beginning of the line. For example: '  obj.define_method :foo'.
        #   The ^(\s+) is to capture the initial whitespace. The \S marks the end of the whitespace (the true "beginning of the line").
        #   The .* is any other junk they want to throw in before we get to the good sweet def that we are looking for!

        # This is the normal case (I *think*).
        # $1 will be set to the whitespace at the beginning of the line.
      else
        # Hmm... Try plan B. Sometimes it is defined like this:
        #          define_method(reflection.name) do |*params|
        #            force_reload = params.first unless params.empty?   # <-- And this is the line they claim we started on. (Liars.)
        # So we have to back up one line and try again...
        line_num -= 1
        first_line = code_for_file[line_num]
        if first_line =~ /^(\s+)\S?.*def/   # (includes define_method)
          # Good
        else
          puts "Warning: Expected #{file}:#{line_num} =~ /def.*#{method}/, but was:" #, but it was:\n#{first_line}"
          column pretty_code_for(@file, @line, ' ', :bold)
          newline
          return
        end
      end
      leading_whitespace = $1

      # Look for ending 'end'
      lines_containing_method =
        (line_num .. [code_for_file.size - 1, line_num+30].min).
        map {|i| [i, code_for_file[i]]}.
        select_until(inclusive = true) do |line_num, line|
          line =~ /^#{leading_whitespace}end/
      end

      common_indentation = lines_containing_method.map { |i, line|
        line =~ /^((  )*)/; $1 ? $1.size : 0
      }.min
      lines_containing_method.map! do |line_num, line|
        [line_num, line.sub(/^#{' ' * common_indentation}/, '')]
      end

      if line_to_highlight
        lines_containing_method.map! do |line_num, line|
          line_to_highlight == line_num ?
            [line_num, plain_indent(indent_adjustment) + '  ' + ' -> '.green + line.bold] :
            [line_num, plain_indent(indent_adjustment) + '  ' + '    ' + line]  
        end
      end

      output = lines_containing_method.
        map {|line_num, line| line}.join

      output
    end
  end # code_for_entire_method
  
  def header_before_code_for_entire_method(file, line_num)
    puts '' + "#{fully_qualified_method} (#{file}:#{line_num}) (#{@event}):".magenta if show_filename_and_line_numbers
  end

  def do_show_locals_verbosely
    variables = Variables.new(:local, @binding)
    if variables.any?
      puts variables.verbose_to_s.yellow
    end
  end


end # class Unroller


class String
  method_space :code_unroller do

    def make_it_fit(max_width, overflow = :chop_right)
      returning(string = self) do
        if string.length_without_color > max_width      # Wider than desired column width; Needs to be chopped.
          unless max_width < 4                          # Is there even enough room for it if it *is* chopped? If not, then don't even bother.
            #Kernel.p overflow
            if overflow == :chop_left
              #puts "making string (#{string.length_without_color}) fit within #{max_width} by doing a :chop_left"
              #puts "chopping '#{string}' at -(#{max_width} - 3) .. -1!"
              chopped_part = string[-(max_width - 3) .. -1]
              string.replace '...' + chopped_part
            elsif overflow == :chop_right
              #puts "making string (#{string.length_without_color}) fit within #{max_width} by doing a :chop_right"
              chopped_part = string[0 .. (max_width - 3)]
              string.replace chopped_part + '...'
            end
          else
            string = ''
          end
        end
      end
    end

  end
end # class String


# Make it really, really easy and concise, for those who like it that way.
module Kernel
  def tron(*args)
    Unroller::trace_on(*args)
  end
  def troff(*args)
    Unroller::trace_off(*args)
  end
end

















#  _____         _
# |_   _|__  ___| |_
#   | |/ _ \/ __| __|
#   | |  __/\__ \ |_
#   |_|\___||___/\__|
#
# :todo: These tests seriously need to be converted into automated tests. But I was lazy/in a hurry when I wrote this...
# It would be kind of cool if we could simultaneously *capture* the output (for asserting things against in automated tests) and
# *display* the output (so that we can manually check for *visual*/stylistic/color regression problems that automated tests 
# couldn't really judge ... and because it's just pretty to look at :) ).

if $0 == __FILE__
  require 'test/unit'

  def herald(message)
    puts message.ljust(130).bold.white.on_magenta
  end

  Unroller::display_style = :concise #:show_entire_method_body

  class TheTest < Test::Unit::TestCase
    def test_1
    end
  end


  herald '-----------------------------------------------------------'
  herald 'Can call trace_off even if not tracing'
  herald '(Should see no output)'
  Unroller::trace_off

  herald '-----------------------------------------------------------'
  herald 'Can call exclude even if not tracing'
  herald '(Should see no output)'
  Unroller::exclude

  herald '-----------------------------------------------------------'
  herald 'Testing return value (should print 3 in this case)'
  puts Unroller::trace { 1 + 2 }

  herald '-----------------------------------------------------------'
  herald 'Simple test'
  def jump!(how_high = 3)
    how_high.times do
      'jump!'
    end
  end
  Unroller::trace
  jump!(2)
  Unroller::trace_off

  herald '-----------------------------------------------------------'
  herald "Testing that this doesn't trace anything (condition == false proc)"
  $trace = false
  Unroller::trace(:condition => proc { $trace }) do
    jump!
  end

  herald '-----------------------------------------------------------'
  herald "Testing that this doesn't trace the inner method (method2), but does trace method1 and method3 (exclude)"
  def method1; end
  def method2
    'stuff!'
  end
  def method3; end
  Unroller::trace do
    method1
    Unroller::exclude do
      method2
    end
    method3
  end

  herald '-----------------------------------------------------------'
  herald "Testing that we can try to turn tracing on even if it's already on"
  def method1
    Unroller::trace
    v = 'in method1'
  end
  Unroller::trace do
    Unroller::trace do
      v = 'about to call method1'
      method1
    end
    v = "We've exited out of one trace block, but we're still within a trace block, so this line should be traced."
  end


  herald '-----------------------------------------------------------'
  herald 'Test with block; very deep (test for over-wide columns)'
  ('a'..last='y').each do |method_name|
    next_method_name = method_name.next unless method_name == last
    eval <<-End, binding, __FILE__, __LINE__ + 1
      def _#{method_name}
        #{next_method_name && "_#{next_method_name}"}
      end
    End
  end
  Unroller::trace(:depth => 5) do
    _a
  end

  herald '-----------------------------------------------------------'
  herald 'Test watching a call stack unwind (only)'
  ('a'..last='y').each do |method_name|
    next_method_name = method_name.next unless method_name == last
    eval <<-End, binding, __FILE__, __LINE__ + 1
      def _#{method_name}
        #{next_method_name && "_#{next_method_name}"}
        #{'Unroller::trace(:depth => caller(0).size)' if method_name == last }
      end
    End
  end
  _a
  Unroller::trace_off


  herald '-----------------------------------------------------------'
  herald 'Testing :depth => :use_call_stack_depth'
  def go_to_depth_and_call_1(depth, &block)
    #puts caller(0).size
    if caller(0).size == depth
      puts 'calling a'
      block.call
    else
      go_to_depth_and_call_2(depth, &block)
    end
    #puts caller(0).size
  end
  def go_to_depth_and_call_2(depth, &block)
    #puts caller(0).size
    if caller(0).size == depth
      puts 'calling a'
      block.call
    else
      go_to_depth_and_call_1(depth, &block)
    end
    #puts caller(0).size
  end
  ('a'..last='c').each do |method_name|
    next_method_name = method_name.next unless method_name == last
    eval <<-End, binding, __FILE__, __LINE__ + 1
      def _#{method_name}
        #{next_method_name && "_#{next_method_name}"}
      end
    End
  end
  go_to_depth_and_call_1(14) do
    Unroller::trace(:depth => :use_call_stack_depth) do
      _a
    end
  end

  herald '-----------------------------------------------------------'
  herald 'Testing without :depth => :use_call_stack_depth (for comparison)'
  go_to_depth_and_call_1(14) do
    Unroller::trace() do
      _a
    end
  end

  herald '-----------------------------------------------------------'
  herald "Test max_depth 5: We shouldn't see the calls to f, g, ... because their depth > 5"
  ('a'..last='y').each do |method_name|
    next_method_name = method_name.next unless method_name == last
    eval <<-End, binding, __FILE__, __LINE__ + 1
      def _#{method_name}
        #{next_method_name && "_#{next_method_name}"}
      end
    End
  end
  Unroller::trace(:max_depth => 5) do
    _a
  end

  herald '-----------------------------------------------------------'
  herald 'Test with long filename (make sure it chops it correctly)'
  File.open(filename = '_code_unroller_test_with_really_really_really_really_really_really_really_really_really_long_filename.rb', 'w') do |file|
    file.puts "
      def sit!
        jump!
      end
    "
  end
  load filename
  Unroller::trace(:depth => 5) do
    sit!
  end
  require 'fileutils'
  FileUtils.rm filename

  herald '-----------------------------------------------------------'
  herald 'Test @max_lines'
  ('a'..last='h').each do |method_name|
    next_method_name = method_name.next unless method_name == last
    eval <<-End, binding, __FILE__, __LINE__ + 1
      def _#{method_name}
        #{next_method_name && "_#{next_method_name}"}
      end
    End
  end
  Unroller::trace(:max_lines => 20) do
    _a
  end

  herald '-----------------------------------------------------------'
  herald 'Test :line_matches'
  herald 'Should only see lines matching "$a_global"'
  require 'facets/string/to_re'
  Unroller::trace :line_matches => '$a_global'.to_re do
    # This won't print anything, because for evals that are missing the __FILE__, __LINE__ arguments, we can't even read the source code (unfortunately)
    eval %(
    whatever = 'whatever'
    $a_global = '1st time'
    foo = 'foo'
    $a_global = '2nd time'
    blah = 'blah'
    )

    # This should print 2 lines:
    whatever = 'whatever'
    $a_global = '1st time'
    foo = 'foo'
    $a_global = '2nd time'
    blah = 'blah'
  end

  herald '-----------------------------------------------------------'
  herald 'Test :file_match'
  herald 'Should only see calls to in_this_file'
  require_local '../test/other_file'
  def in_this_file
    a = 'a'
    b = 'b'
    c = 'c'
    etc = 'etc.'
  end
  Unroller::trace(:file_match => __FILE__) do
    in_this_file()
    in_other_file()
  end

  herald '-----------------------------------------------------------'
  herald 'Test :dir_match'
  herald 'Should only see calls to in_this_file'
  Unroller::trace(:dir_match => __FILE__) do
    in_this_file()
    in_other_file()
  end

  herald '-----------------------------------------------------------'
  herald 'Test @exclude_methods'
  herald 'Should only see calls to green, strong'
  class Wasabi #:nodoc:
    def green
      '...'
    end
    def too_green
      '...'
    end
    def strong
      '...'
    end
    def too_strong
      '...'
    end
  end
  wasabi = Wasabi.new
  Unroller::trace(:exclude_methods => /too_/) do
    wasabi.green
    wasabi.too_green    # Censored, sucka!
    wasabi.strong
    wasabi.too_strong   # Censored, sucka!
  end

  herald '-----------------------------------------------------------'
  herald 'Test @exclude_methods with symbol'
  herald "Should should see too_strong, because that doesn't exactly match the exclusion regexp"
  Unroller::trace(:exclude_methods => :too_) do
    wasabi.too_strong
  end

  herald '-----------------------------------------------------------'
  herald 'Test @exclude_classes'
  herald 'Should only see calls to Interesting::...'
  class Interesting #:nodoc:
    def self.method
      '...'
    end
    def method
      '...'
    end
  
  end
  module Uninteresting #:nodoc:
  
    class ClassThatCluttersUpOnesTraces #:nodoc:
      ('a'..last='h').each do |method_name|
        next_method_name = method_name.next unless method_name == last
        eval <<-End, binding, __FILE__, __LINE__ + 1
          def _#{method_name}
            #{next_method_name && "_#{next_method_name}"}
            #{'Interesting::method' if method_name == last }
            #{'Interesting.new.method' if method_name == last }
          end
        End
      end
    end
  end
  def create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces
    Uninteresting::ClassThatCluttersUpOnesTraces.new._a
  end
  Unroller::trace(:exclude_classes => Uninteresting::ClassThatCluttersUpOnesTraces) do
    create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces
  end

=begin HistoricalNote
    # This test used to generate output more like this (note the extreme indenting):

     |    create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces  | unroller.rb:645
     |  + calling Object::create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces
     |  / def create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces  | unroller.rb:641
     |  |    Uninteresting::ClassThatCluttersUpOnesTraces.new.a             | unroller.rb:642
     |  |  |  |  |  |  |  |  |  |  + calling Interesting::method
     |  |  |  |  |  |  |  |  |  |  / def self.method                        | unroller.rb:620
     |  |  |  |  |  |  |  |  |  |  |    '...'                               | unroller.rb:621
     |  |  |  |  |  |  |  |  |  |  \ end (returning from Interesting::method)  | unroller.rb:622
     |  |  |  |  |  |  |  |  |  |  + calling Interesting::method
     |  |  |  |  |  |  |  |  |  |  / def method                             | unroller.rb:623
     |  |  |  |  |  |  |  |  |  |  |    '...'                               | unroller.rb:624
     |  |  |  |  |  |  |  |  |  |  \ end (returning from Interesting::method)  | unroller.rb:625
     |  \ end (returning from )    

    # ... which is probably a technically more accurate picture of the current call stack. However, it looked kind of unnatural
    # with all those "extra" indents in there. It was also a waste of horizontal screen space.

    # This changed when the idea of an @internal_depth separate from the @depth (that the user sees) was introduced.

     |    create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces  | unroller.rb:655
     |  + calling Object::create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces
     |  / def create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces  | unroller.rb:651
     |  |    Uninteresting::ClassThatCluttersUpOnesTraces.new.a             | unroller.rb:652
     |  |  + calling Interesting::method
     |  |  / def self.method                                                | unroller.rb:630
     |  |  |    '...'                                                       | unroller.rb:631
     |  |  \ end (returning from Interesting::method)                       | unroller.rb:632
     |  |  + calling Interesting::method
     |  |  / def method                                                     | unroller.rb:633
     |  |  |    '...'                                                       | unroller.rb:634
     |  |  \ end (returning from Interesting::method)                       | unroller.rb:635
     |  \ end (returning from Object::create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces)  | unroller.rb:653

    Much cleaner looking...
=end

  herald '-----------------------------------------------------------'
  herald 'Now let\'s be recursive! We should *not* see any calls to Interesting::* this time. But after we return from it, we should see the call to jump!'
  Unroller::trace(:exclude_classes => [Unroller::ClassExclusion.new('Uninteresting', :recursive)]) do
    create_an_instance_of_UninterestingClassThatCluttersUpOnesTraces
    jump!
  end


  herald '-----------------------------------------------------------'
  herald 'Test class definition'
  Unroller::trace do
    class NewClass #:nodoc:
      def hi
        'hi'
      end
    end
  end


  herald '-----------------------------------------------------------'
  herald 'Test rescuing exception'
  def raise_an_error
    raise 'an error'
  end
  Unroller::trace do
    raise_an_error
  end rescue nil

  herald '-----------------------------------------------------------'
  herald 'Demonstrate how :if condition is useful for local variables too (especially loops and iterators)'
  (1..6).each do |i|
    Unroller::trace :if => proc { 
      if (3..4).include?(i)
        puts "Yep, it's a 3 or a 4. I guess we'll enable the tracer for this iteration then...:"
        true
      end 
    } do
      puts "i is now equal to #{i}"
    end
  end

  herald '-----------------------------------------------------------'
  herald 'Testing the :rails "preset"'
  module ActiveSupport #:nodoc:
    def self.whatever
      'whatever'
    end
  end
  module ActiveMongoose #:nodoc:
    def self.whatever
      'whatever'
    end
  end
  Unroller::trace :preset => :rails do
    ActiveSupport.whatever
    ActiveMongoose.whatever
    ActiveSupport.whatever
  end

  herald '-----------------------------------------------------------'
  herald 'Testing the :dependencies "preset"'
  module ActiveSupport #:nodoc:
    module Dependencies #:nodoc:
      def self.whatever
        'whatever'
      end
    end
  end
  module Gem #:nodoc:
    def self.whatever
      'whatever'
    end
  end
  Unroller::trace :preset => :dependencies do
    ActiveSupport::Dependencies.whatever
    Gem.whatever
  end

  herald '-----------------------------------------------------------'
  herald 'Testing showing local variables'
  def sum(a, b, c)
    a + b + c
  end
  def my_very_own_method
    Unroller::trace :show_args => false, :show_locals => true do
      sum = sum(1, 2, 3)
      sum
      sum = sum(3, 3, 3)
    end
  end
  my_very_own_method

  herald '-----------------------------------------------------------'
  herald 'Testing watch_for_added_methods'
  class Foo #:nodoc:
    def existing_method; end
  end
  Unroller::watch_for_added_methods(Foo, /interesting/) do
    class Foo #:nodoc:
      def foo; end
      def an_interesting_method; end
    end
  end

  herald '-----------------------------------------------------------'
  herald 'Testing :display_style => :show_entire_method_body'
  def bagel
    '...'
  end
  Unroller::trace :display_style => :show_entire_method_body do
    bagel
  end

  herald '-----------------------------------------------------------'
  herald 'Testing :show_filename_and_line_numbers => false'
  'foo'; def bagel
    '...'
  end
  Unroller::trace :display_style => :show_entire_method_body, :show_filename_and_line_numbers => false do
    bagel
  end

  herald '-----------------------------------------------------------'
  herald 'Testing that it finds the definition block even when we use define_method instead of the normal def'
  (class << self; self; end).send :define_method, :big_ol_gaggle do
    '...'
  end
  Unroller::trace :display_style => :show_entire_method_body do
    big_ol_gaggle
  end

#  herald '-----------------------------------------------------------'
#  herald 'Testing :interactive => true / interactive debugger'
#  def factorial(x)
#    if x == 1
#      x
#    else
#      x * factorial(x - 1)
#    end
#  end
#  Unroller::debug do
#    factorial(4)
#  end

  herald '-----------------------------------------------------------'
  herald 'Testing :interactive => true / interactive debugger: two calls on the same line'
  def method_1
    'stuff'
  end
  def method_2
    'stuff'
  end
  # Step Over will step over the call to method_1 but will immediately step into method_2, without coming back to this line to ask you what you want to do about the call to method_2 ...
  # In other words, there is no intermediate 'line' event between the 2 'call' events (right?)
  # This could be confusing to some users... Should we hack it to somehow detect that there are two calls in a row for the same 
  # line and artificially inject a pseudo-line event in between so that we have a chance to show the menu again??
  Unroller::debug do
    sum = method_1 + method_2
    puts sum
  end

  herald '-----------------------------------------------------------'
  herald 'Testing :interactive => true / interactive debugger: with blocks'
  def block_taker(&block)
    puts "I'm the block taker. I take blocks."
    puts "Please Step Over the next line."
    yield   # buggy! it keeps stopping at every line in the yielded block, but shouldn't. 
    # false:: (unroller.rb:1433) (line): ??
    puts "Done yielding to the block"
  end
  Unroller::debug do
    puts "If you do a Step Over, will it go into the block or not? Nope. Because we skipped tracing the yield."
    # to do: have a separate "step over method body but into block" option??
    # that way, it you just want to stay in this local file and avoid seeing the code that *wraps* the call to the block, you could do so...
    block_taker do
      puts 'Pick me! Pick me!'
      puts 'I say! When will I ever be executed?'
      puts 'Oh dear, will they just skip right over me?'
      puts 'Or will they Step In here and take a look?'
    end
  end

  herald '-----------------------------------------------------------'
  herald 'Testing :interactive => true / interactive debugger'
  def subway
    question = 'What kind of bread would you like?'
    bread!
    question = 'Would you like any cheese?'
    cheese!
    question = 'Would you like pickles on that?'
    pickles!
  end
  def bread!
    _do = nil
    _do = nil
    _do = nil
  end
  def cheese!
    _do = nil
    _do = nil
    _do = nil
  end
  def pickles!
    _do = nil
    _do = nil
    _do = nil
  end
  #Unroller::trace :display_style => :show_entire_method_body, :interactive => true do
  Unroller::debug do
    subway
  end

  herald 'End of non-automated tests'
  herald '-----------------------------------------------------------'

end # if $0 == __FILE__ (Tests)

# Set terminal_attributes back to how we found them...
# (This doesn't work so great when you do tracing from within a test... Why not? Because Test::Unit is itself *started* using
# an at_exit callback. Which means (since those callbacks are called in reverse order) that *this* at_exit is called, and *then*
# the test's at_exit is called... So in other words, we end up 'reverting' these attributes before we've even had a chance to
# use the 'new' attributes!)
#at_exit { puts 'Setting terminal_attributes back to how we found them...'; Termios.tcsetattr(STDIN, 0, save_terminal_attributes) }
