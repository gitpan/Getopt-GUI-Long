package Getopt::GUI::Long;

# Copyright 2005-2006, Sparta, Inc.
# All rights reserved.

use strict;
use Text::Wrap;
use Getopt::Long qw();
use QWizard;
use QWizard::Storage::File;
use QWizard::Plugins::Bookmarks qw(init_bookmarks);
use File::Temp qw(tempfile);

our $VERSION="0.62";

require Exporter;

our @ISA = qw(Exporter Getopt::Long);
our @EXPORT = qw(GetOptions);
our $GUI_qw;
our $verbose;
our @ARGVsaved;

#
# Configuration variables
#
my %config = (
	      capture_output => 0,
	      display_help => 0,
	     );

# Primary storage for QWizard
my %primaries = 
  (
   display_results =>
   {
    title => "$ARGV Output",
    questions =>
    [
     { type => 'paragraph',
       values => sub {
	   my $data;
	   # XXX: arg.  optimize with read() and then a file display widget
	   open(I,$config{'output_file'});
	   while (<I>) {
	       $data .= $_;
	   }
	   close(I);
	   return $data;
       }
     }
    ]
   }
  );


#
# changes the default for a given question to a memorized value
#
sub set_default_opt {
    my ($cache, @qs) = @_;
    if ($cache) {
	foreach my $q (@qs) {
	    if (!$q->{'default'}) {
		$q->{'default'} = $cache->get($q->{'name'});
	    }
	}
    }
    return @qs;
}

sub GetOptions(@) {
    my ($st, $cb, @opts) = ((ref($_[0]) eq 'HASH') ?
			    (1, 1, @_) : (0, 2, @_));
    my @savedopts = @_;

    my $progname = $main::0;
    my $cache;
    my %GUI_info;
    my %addpris;

    $progname =~ s/([^a-zA-Z0-9])/'_' . ord($1)/eg;
    my $cachefile = "$ENV{HOME}/.GetoptLongGUI/$progname";
    my $marksfile = "$ENV{HOME}/.GetoptLongGUI/$progname-bookmarks";
    if (-f $cachefile) {
	$cache = new QWizard::Storage::File (file => $cachefile);
	$cache->load_data();
    }

    # If the user didn't specify any arguments, we display a GUI for them.
    if ($#main::ARGV == -1) {

	$config{'GUI_on'} = 1;
	
	# they called us with no options, offer them a GUI screen full
	my @names;
	my %names;
	my @qs;
	my $screencount = 0;
	my @saveqs;
	my $pris;
	
	# Define the first screen
	$pris->{'screen' . $screencount} =
	  { title => "Options for $main::0", };

	for (my $i = $st; $i <= $#opts; $i += $cb) {
	    my ($spec, $desc, %rest);
	    if (ref($opts[$i]) eq 'ARRAY') {
		$spec = $opts[$i][0];
		$desc = $opts[$i][1];
		%rest = @{$opts[$i]}[2..$#{$opts[$i]}];
	    } elsif ($opts[$i] eq '') {
		push @qs,"";
		next;
	    } else {
		$spec = $opts[$i];
	    }

	    if ($spec =~ /^GUI:(.*)/) {
		my $guiname = $1;
		
		if ($guiname eq "guionly") {
		    #
		    # a specific QWizard question to be added
		    #
		    push @qs,
		      set_default_opt($cache,@{$opts[$i]}[1..$#{$opts[$i]}]);

		} elsif ($guiname eq "separator") {
		    #
		    # add a separator label.
		    #
		    push @qs, "", { type => 'label', text => $opts[$i][1]};

		} elsif ($guiname eq "screen") {
		    #
		    # Start a brand new screen
		    #

		    # If the screen is optional via a previous flag
		    if (exists($rest{'doif'}) &&
			ref($rest{'doif'}) ne 'CODE') {
			$rest{'doifstr'} = $rest{'doif'};
			$rest{'doif'} = 
			  sub { return qwparam($_[2]{'doifstr'})};
		    }

		    # Define the new screen
		    if ($#qs == -1) {
			# screen definition occured before the first
			# question, have this only affect the current
			# screen.
			foreach my $tok (keys(%rest)) {
			    $pris->{'screen' . $screencount}{$tok} = 
			      $rest{$tok};
			}
			# maybe set the title
			$pris->{'screen' . $screencount}{'title'} = $desc
			  if ($desc);
		    } else {
			$pris->{'screen' . $screencount}{'questions'} = [@qs];
			$screencount++;
			$pris->{'screen' . $screencount} =
			  { title => $desc || "Options for $main::0",
			    # allow user to override
			    %rest };
			@qs = ();
		    }
		} else {
		    # generic GUI config token; store the name
		    @{$GUI_info{$guiname}} = @{$opts[$i]}[1..$#{$opts[$i]}];
		}
		next;
	    }

	    # regexp stolen (and modified) from Getopt::Long
	    # XXX: copy copyright
	    $spec =~ m;^
		   # $1 = whole name pattern
                   (
                     # $2 = Option name
                     (\w+[-\w]* )
                     # $3 = first name, or "?"
                     (?: \| (\? | \w[-\w]* )? )?
                     # Alias names, or "?"
                     (?: \| (?: \? | \w[-\w]* )? )*
                   )?
                   # $4 = arguments
                   (
                     # Either modifiers ...
                     [!+]
                     |
                     # ... or a value/dest specification
                     [=:] [ionfs] [@%]?
                     |
                     # ... or an optional-with-default spec
                     : (?: -?\d+ | \+ ) [@%]?
                   )?
		       $;x;

	    # map option pieces into variables
	    my ($pat, $optname, $firstname, $args) = ($1, $2, $3, $4);


	    # calculate the option name to use from the list of names.
	    my $name = (length($optname) == 1 ? 
			((length($firstname) > 1) ? $firstname : $optname) :
			$optname);

	    # if no description, we copy the name and use that.
	    $desc = $name if (!$desc);

	    # remember the name of the option we're working on
	    push @names,$name;
	    $names{$name} = $args;

	    # get default value from the user preference if they had
	    # one already.
	    my $defval;
	    $defval = $cache->get($name) if ($cache);

	    # calculate a default value based on the passed in hash
	    # ref or variable reference.
	    if (!$defval) {
		$defval = (($cb == 1) ? 
			   (exists($_[0]{$optname}) ? $_[0]{$optname} : undef) :
			   $opts[$i+1]);
	    }

	    #
	    # Now constructure the needed GUI question
	    #
	    if ($rest{'question'}) {
		# if a QWizard question definition was passed to us, use that.
		if (ref($rest{'question'}) ne 'ARRAY') {
		    $rest{'question'} = [$rest{'question'}];
		}
		map {
		    $_->{'name'} = $name if ($_->{'name'} eq '');
		    $_->{'text'} = $desc if ($_->{'text'} eq '');
		    $_->{'default'} = $defval if (!exists($_->{'default'}));
		} @{$rest{'question'}};
		push @qs, set_default_opt($cache, @{$rest{'question'}});

	    } elsif ($desc =~ /^!GUI/) {
		# if the description was marked as not-for-the-GUI, don't
		# dispaly aynthing and skip it but remember the parameter name.
		push @qs, {type => 'hidden', name => $name, default => $defval};

	    } elsif (!$args || $args =~ /[!+]/) {
		#
		# Boolean => Checkbox
		#
		push @qs,
		  {
		   'text' => $desc,
		   'type' => 'checkbox',
		   values => [1, undef],
		   default => (($defval)?1:undef),
		   'name' => $name,
		  };

	    } elsif ($args =~ /i/) {
		#
		# Integer => textbox with int check
		#
		push @qs,
		  {
		   'text' => $desc,
		   'type' => 'text',
		   default => $defval,
		   'name' => $name,
		   check_value => (($rest{'required'}) ?
				   \&qw_integer : \&qw_optional_integer),
		  };
	    } elsif ($args =~ /[sf]/) {
		#
		# Float/string => textbox
		#
		# XXX: deal with floats seperately
		push @qs,
		  {
		   'text' => $desc,
		   'type' => 'text',
		   default => $defval,
		   'name' => $name,
		  };
	    } else {
		#
		# Unknown => textbox
		#
		push @qs,
		  {
		   'text' => $desc,
		   'type' => 'text',
		   default => $defval,
		   'name' => $name,
		  };
	    }

	    # if the required flag was set, force a requirement check.
	    if ($rest{'required'} && !exists($qs[$#qs]{'check_answer'})) {
		$qs[$#qs]{'check_value'} = \&qw_required_field;
	    }
	}

	# include other primary information passed to us
	if (exists($GUI_info{'otherprimaries'})) {
	    %addpris = @{$GUI_info{'otherprimaries'}};
	}

	# Prompt for remaining arguments (or don't if requested to
	# skip it).  Normally this would be stuff handled beyond the
	# realm of the Getopt processing like file-names, etc.
	if ($GUI_info{'nootherargs'}) {
	    push @{$pris->{'screen0'}{'questions'}},
	      { type => 'hidden', name => '__otherargs' };
	} else {
	    my $q = { type => 'text',
		      width => 80,
		      name => '__otherargs',
		      default => 
		      ($cache) ? $cache->get('__otherargs') : '',
		      text =>
		      $GUI_info{'otherargs_text'} || "Other Arguments",
		    };
	    if ($GUI_info{'otherargs_required'}) {
		$q->{'check_value'} = \&qw_required_field;
	    }
	    push @{$pris->{'screen0'}{'questions'}}, "", $q;
	  }

	# our last primary setup
	$pris->{'screen' . $screencount}{'questions'} = [@qs];
	push @{$pris->{'screen' . $screencount}{'actions'}},
	  [sub {
	       #
	       # map all the defined option names from
	       # qwparam values pulled from the GUI back
	       # into ARGV options to be passed to
	       # Getopt::Long for processing
	       #
	       my ($qw, $name_array, $name_hash) = @_;
	       for (my $i = 0; $i <= $#{$name_array}; $i++) {
		   if (qwparam($name_array->[$i])) {
		       if (!$name_hash->{$name_array->[$i]} ||
			   $name_hash->{$name_array->[$i]} =~ /[+!]/) {
			   push @main::ARGV,
			     "--" . $name_array->[$i];
		       } else {
			   push @main::ARGV,
			     "--" . $name_array->[$i],
			       qwparam($name_array->[$i]);
		       }
		   }
	       }
	       push @main::ARGV,
		 split(/\s+/,qwparam('__otherargs'));
	       return 'OK';
	   }, \@names, \%names];

	# add in later screens if needed
	if ($screencount > 0) {
	    push @{$pris->{'screen0'}{'post_answers'}},
	      [sub {
		   my ($wiz, $screencount) = @_;
		   for (my $i = $screencount; $i >= 1; $i--) {
		       $wiz->add_todos('screen' .$i);
		   }
	       }, $screencount];
	}
	
	# add in an post_answers clauses to the master primary
	# XXX: move to each screen def
	if ($GUI_info{'post_answers'}) {
	    push @{$pris->{'screen0'}{'post_answers'}},
	      @{$GUI_info{'post_answers'}};
	}

	# add in an actions clauses to the master primary
	if ($GUI_info{'actions'}) {
	    unshift @{$pris->{'screen0'}{'actions'}}, @{$GUI_info{'actions'}};
	}

	# Finally, construct the QWizard GUI class...
	my $qw = new QWizard(primaries => $pris,
			     no_confirm => 1,
			     title => $main::0);

	# add the bookmarks menu
	if (ref($qw->{'generator'}) !~ /HTML/ && !$GUI_info{'nosavebutton'}) {
	    my $marks = new QWizard::Storage::File (file => $marksfile);
	    $marks->load_data();
	    init_bookmarks($qw, $marks, title => 'Save Settings');
	}

	# ... remember it ...
	$GUI_qw = $qw;

	# ... load in our other primaries ...
	$qw->merge_primaries(\%primaries);
	$qw->merge_primaries(\%addpris);

	# ... and tell it to go
	$qw->magic('screen0', @{$GUI_info{'submodules'}});


	# ... if we aren't finished processing then exit.  This should
	# only happen if we're in a CGI script where we're not done yet.
	if (($qw->{'state'} != $QWizard::states{'ACTING'} &&
	     $qw->{'state'} != $QWizard::states{'FINISHED'}) ||
	    $qw->{'state'} == $QWizard::states{'CANCELED'}) {
	    # we're not done or have been cancelled!  exit!
	    exit;
	}

	
	# if there are any final hook routines to call, do so.
	if ($GUI_info{'hook_finished'}) {
	    foreach my $h (@{$GUI_info{'hook_finished'}}) {
		$h->();
	    }
	}
    }

    # map the options passed to us to ones that are compliant with
    # Getopt::Long
    @opts = MapToGetoptLong(\%GUI_info, @_);

    # save the arguments we contstructed for future use.
    @ARGVsaved = @main::ARGV;

    # display the results to the user if verbose was requested.
    if ($verbose) {
	print STDERR "passing $#main::ARGV args to Getopt::Long: ",
	  join(",",@main::ARGV),"\n";
    }

    # finally, pass on to the original Getopt::Long routine.
    my $ret = Getopt::Long::GetOptions(@opts);

    # capture stderr/out if required to display later.
    if ($config{'capture_output'} && $config{'GUI_on'}) {
	($config{'fh'}, $config{'output_file'}) =
	  tempfile("guilongXXXXXX", SUFFIX => 'txt');
	close(STDOUT);
	close(STDERR);
	*STDOUT = $config{'fh'};
	*STDERR = $config{'fh'};
    }

    if (defined($config{'display_help'}) && $config{'display_help'} &&
	ref($_[0]) eq 'HASH' &&
	($_[0]{'help'} || $_[0]{'help-full'} || $_[0]{'h'})) {
	display_help($st, $cb, \%GUI_info, \@savedopts, @_);
    }

    if (defined($config{'display_help'}) && $config{'display_help'} &&
	defined($GUI_info{'VERSION'}) &&
	ref($_[0]) eq 'HASH' && ($_[0]{'version'})) {
	print STDERR "Version: ",$GUI_info{'VERSION'},"\n";
	exit(0);
    }
    return $ret;
}

#
# prints help information
#
sub display_help {
    my $st = shift;
    my $cb = shift;
    my $GUI_info = shift;
    my $savedopts = shift;
    # Print help message if auto_help is on (XXX)

    print STDERR "Usage: $0 [OPTIONS] ",
      ($GUI_info->{'otherargs_text'} ||
       ($GUI_info->{'nootherargs'} ? "" : "Other Arguments")),
	 "\n\nOPTIONS:\n";

    for (my $i = $st; $i <= $#{$savedopts}; $i += $cb) {
	my ($spec, $desc, %rest);
	if (ref($savedopts->[$i]) eq 'ARRAY') {
	    $spec = $savedopts->[$i][0];
	    $desc = $savedopts->[$i][1];
	    %rest = @{$savedopts->[$i]}[2..$#{$savedopts->[$i]}];
	} elsif ($savedopts->[$i] eq '') {
	    print STDERR "\n";
	    next;
	} else {
	    $spec = $savedopts->[$i];
	}

	# print separators
	if ($spec =~ /^GUI:(separator|screen)/) {
	    print STDERR "\n$desc\n";
	    next;
	}

	next if ($spec =~ /^GUI:(.*)/);

	# regexp stolen (and modified) from Getopt::Long
	# XXX: copy copyright
	$spec =~ m;^
		   # $1 = whole name pattern
		   (
		   # $2 = Option name
		   (\w+[-\w]* )
		   # $3 = first name, or "?"
		   (?: \| (\? | \w[-\w]* )? )?
		   # Alias names, or "?"
		   (?: \| (?: \? | \w[-\w]* )? )*
		  )?
		   # $4 = arguments
		   (
		   # Either modifiers ...
		   [!+]
		   |
		   # ... or a value/dest specification
		   [=:] [ionfs] [@%]?
		   |
		   # ... or an optional-with-default spec
		   : (?: -?\d+ | \+ ) [@%]?
		  )?
		   $;x;

	# map option pieces into variables
	my ($pat, $optname, $firstname, $argspec) = ($1, $2, $3, $4);

	# turn argspecs into human consumable versions.
	if ($argspec =~ /i/) {
	    $argspec = "INTEGER";
	} elsif ($argspec =~ /f/) {
	    $argspec = "FLOAT";
	} elsif ($argspec =~ /s/) {
	    $argspec = "STRING";
	} else {
	    $argspec = "";
	}

	my @args=split(/\|/,$pat);
	if ($_[0]{'help-full'}) {
	    # they want *everything*
	    for (my $argc=0;  $argc <= $#args; $argc++) {
		_display_help_option($args[$argc], $argspec,
				     ($argc == $#args)?$desc:"");
	    }

	} elsif ($_[0]{'h'}) {
	    # assume short names perferred and always show the shortest
	    _display_help_option($args[0], $argspec, $desc);

	} elsif ($_[0]{'help'}) {
	    # assume long names perferred and use those if possible
	    if ($#args == 0) {
		# only one is avialable
		_display_help_option($args[0], $argspec, $desc);
	    } elsif (length($args[0]) == 1) {
		# first one is short, display the second...
		_display_help_option_long($args[1], $argspec, $desc);
	    } else {
		# first one itself is long, display it.
		_display_help_option_long($args[0], $argspec, $desc);
	    }
	}
    }

    # display the display_help options
    print STDERR "\nHelp Options:\n";
    _display_help_option_short('h','',
			       'Display help options -- short flags preferred');
    _display_help_option_long('help','',
			       'Display help options -- long flags preferred');
    _display_help_option_long('help-full','',
			       'Display all help options -- short and long');
    _display_help_option_long('version','',
			      'Display the version number')
      if ($GUI_info->{'VERSION'});

    exit(1);
}

sub _display_help_option {
    my ($arg,$spec,$desc) = @_;
    if (length($arg) == 1) {
	_display_help_option_short(@_);
    } else {
	_display_help_option_long(@_);
    }
}

sub _display_help_option_short {
    my ($arg,$spec,$desc) = @_;
    printf STDERR ("   -%-20s %-55s\n", $arg . " " . $spec, $desc);
}

sub _display_help_option_long {
    my ($arg,$spec,$desc) = @_;
    printf STDERR ("  --%-20s %-55s\n",
		   $arg . ($spec ? "=" : "") . $spec, $desc);
}

sub MapToGetoptLong {
    my $guiinfo = shift;
    my ($st, $cb, @opts) = ((ref($_[0]) eq 'HASH') 
			    ? (1, 1, $_[0]) : (0, 2));
    for (my $i = $st; $i <= $#_; $i += $cb) {
	if ($_[$i]) {
	    if (ref($_[$i]) eq 'ARRAY' && $_[$i][0] =~ /^GUI:(.*)/) {
		$guiinfo->{$1} = $_[$i][1];
		next;
	    }
	    push @opts, ((ref($_[$i]) eq 'ARRAY') ? $_[$i][0] : $_[$i]);
	    push @opts, $_[$i+1] if ($cb == 2);
	}
    }
    push @opts,GetHelpOptions() if ($config{'display_help'});
    return @opts;
}

sub GetHelpOptions {
    return ('help','h','help-full','version');
}

#
# Configure's the running operation of the module.  Any unknown
# arguments are passed to the parent Configure script.
#
sub Configure (@) {
    my @options = @_;
    my @passopts;
    foreach my $opt (@options) {
	if (exists($config{$opt})) {
	    $config{$opt} = 1;
	} else {
	    push @passopts, $opt;
	}
    }
    Getopt::Long::Configure(@passopts);
}

#
# Calls qwizard at the end if output capture was turned on.
#
sub END {
    if ($config{'capture_output'} && $config{'GUI_on'} && $config{'fh'}) {
	$config{'fh'}->close();
	$GUI_qw->magic('display_results');
	unlink($config{'output_file'});
    }
}

1;

=pod

=head1 NAME

Getopt::GUI::Long

=head1 SYNOPSIS

  use Getopt::GUI::Long;

  # pass useful config options to Configure
  Getopt::GUI::Long::Configure(qw(display_help no_ignore_case capture_output));
  GetOptions(\%opts,
             ["GUI:separator",   "Important Flags:"],
	     ["f|some-flag=s",   "A flag based on a string"],
	     ["o|other-flag",    "A boloean"],
            );

  # or use references instead of a hash (less tested, however):

  GetOptions(["some-flag=s",  "perform some flag based on a value"] => \$flag,
             ["other-flag=s", "perform some flag based on a value"] => \$other);

  # displays auto-help given the input above:

  % opttest -h
  Usage: opttest [OPTIONS] Other Arguments

  OPTIONS:

  Important Flags:
     -f STRING             A flag based on a string
     -o                    A boloean

  Help Options:
     -h                    Display help options -- short flags preferred
    --help                 Display help options -- long flags preferred
    --help-full            Display all help options -- short and long



  # or long help:

  % opttest --help
  Usage: opttest [OPTIONS] Other Arguments

  OPTIONS:

  Important Flags:
    --some-flag=STRING     A flag based on a string
    --other-flag           A boloean

  Help Options:
     -h                    Display help options -- short flags preferred
    --help                 Display help options -- long flags preferred
    --help-full            Display all help options -- short and long

  # or a GUI screen:

  (see http://net-policy.sourceforge.net/images/getopt_example.png )

=head1 DESCRIPTION

  This module is a wrapper around Getopt::Long that extends the value of
  the original Getopt::Long module to:

  1) add a simple graphical user interface option screen if no
     arguments are passed to the program.  Thus, the arguments to
     actually use are built based on the results of the user
     interface. If arguments were passed to the program, the user
     interface is not shown and the program executes as it normally
     would and acts just as if Getopt::Long::GetOptions had been
     called instead.

  2) provide an auto-help mechanism such that -h and --help are
     handled automatically.  In fact, calling your program with -h
     will default to showing the user a list of short-style arguments
     when one exists for the option.  Similarly --help will show the
     user a list of long-style when possible.  --help-full will list
     all potential arguments for an option (short and long both).

  It's designed to make the creation of graphical shells trivial
  without the programmer having to think about it much as well as
  providing automatic good-looking usage output without the
  programmer needing to write usage() functions.

=head1 HISTORY

This module was originally named Getopt::Long::GUI but the
Getopt::Long author wanted to reserve the Getopt::Long namespace
entirely for himself and thus it's been recently renamed to
Getopt::GUI::Long instead.  The class tree isn't as clean this way, as
this module still inherits from Getopt::Long but it everything still
works of course.

=head1 USAGE

The Getopt::GUI::Long module can work identically to the Getopt::Long
module but really benefits from some slightly different usage
conventions described below.

=head2 Option format:

Option strings passed should be formatted in one of the following ways:

=over

=item Empty String

Empty strings are ignored by the non-GUI version of the command, but
are treated as vertical separators between questions when displaying
the GUI screen.

=item Standard flag specification string

   EG: "some-flag|optional-flag=s"

This is the standard method by which Getopt::Long does things and is
merely treated the same way here.  In this case, the text presented to
the user screen will be the first name in the list ("some-flag") in
the above option.  The type of wdget displayed with the text will
depend on the optional =s/i/whatever flag and will be either a
checkbox, entry box, ...

=item Array Reference

   EG: ["some-flag|optional-flag=s", 'Prompt text', OTHER],

The values passed in this array are as follows:

=over

=item 0: Standard flag specification string

Same as always, and as above.

=item 1: Prompt text about flag

The help text that should be shown to the user in the graphical
interface.  In the example above rather than "some-flag" being shown,
"Prompt text" will be shown next to the widget instead.

If the prompt text is equal to "!GUI" then this option will not be
displayed (automatically at least) within the GUI.

=item 2...: OTHER options

Beyond the name and description, key value pairs can indicate more
about how the option should be handled.

=over

=item required => 1

Forces a screen option to be filled out by the user.

=item question => { QUESTION FORM }

=item question => [{ QUESTION1}, {QUESTION2}, ...]

These allows you to build custom QWizard widgets to meet a particular
question need.  It's highly useful for doing menus and single choice
fields that normally command line options don't handle well.  For
example consider a numeric priority level between 0 and 10.  The
following question definition will give them a menu rather than a fill
in the blank field:

  ['priority=i','Priority Level',
   question => { type => 'menu', values => [1..10] }]

Note you can specify multiple question widgets if needed as well,
though this will probably be rare in usage.

=back

[others TBD]

=back

=back

=head1 Special Flag Names

Flags that start with GUI: are not passed to the normal Getopt::Long
routines and are instead for internal GUI digestion only.  If the GUI
screen is going to be displayed (remember: only if the user didn't
specify any options), these extra options control how the GUI behaves.

Some of these options requires some knowledge of the QWizard
programming system.  Knowledge of QWizard should only be required if
you want to make use of those particular extra features.

=over

=item GUI:guionly

  EG:  ['GUI:guionly', { type => 'checkbox', name => 'myguiflag'}]

Specifies a valid QWizard question(s) to only be shown when the gui is
displayed, and the specification is ignored during normal command line
usage.

=item GUI:separator

  EG:  ['GUI:separator', 'Task specific options:']

Inserts a label above a set of options to identify them as a group.

=item GUI:screen

  EG:  ['GUI:screen', 'Next Screen Title', ...]

Specifies that a break in the option requests should occur and the
remaining options should appear on another screen(s).  This allows
applications with a lot of options to reduce the complexity it offers
users and offers a more "wizard" like approach to helping them decide
what they're trying to do.

You can also make this next screen definition conditional by defining
an earlier option that may be, say, a boolean flag called
"feature_flag".  Using this you can then only go into the next screen
if the "feature_flag" was set by doing the following:

        ['feature-flag',
         'turn on a special feature needing more options'],

        ['GUI:screen', 'Next Screen Title', doif => 'feature-flag']
        ['feature-arg1', 'extra argument #1 for special feature'],
        ...

Also, if you need to do more complex calculations use the qwparam()
function of the passed in QWizard object in a subroutine reference instead:

        ['feature1','Turn on feature #1"],
        ['feature2','Turn on feature #2"],

        ['GUI:screen', 'Next Screen Title', 
         doif => sub { 
           return ($_[1]->qwparam('feature1') && $_[1]->qwparam('feature2'));
        }]

=item GUI:otherargs_text

  EG:  ['GUI:otherargs', 'Files to process:']

Normally the GUI screen shows a "Other Arguments:" option at the
bottom of the main GUI screen to allow users to entry additional flags
(needed for file names, etc, and other non-option arguments to be
passed).  However, since it doesn't know what these arguments should
be it can only provide a generic "Other Arguments:" description.  This
setting lets you change that text to something specific to what your
application experts, such as "Files:" or "HTML Files:" or something
that helps the user understand what is expected of them.

If you want to self-handle the argument prompting using other QWizard
constructs, then use the nootherargs token described below instead.

=item GUI:nootherargs

  EG:  ['GUI:nootherargs', 1]

Normally the GUI screen shows a "Other Arguments:", or programmer
described text as described above, option at the bottom of the main
GUI screen.  If you're going to handle the additional arguments
yourself in some way (using either some GUI:guionly or
(GUI:otherprimaries and GUI:submodules) flags), then you should
specify this so the other arguments field is not shown.  You're
expected, in your self-handling code, to set the __otherargs QWizard
parameter to the final arguments that should be passed on.

=item GUI:nosavebutton

  EG:  ['GUI:nosavebutton', 1]

Normally the GUI screen offers a "save" menu that lets users save
their current screen settings for future calls.  Using this setting
turns off this behavior so the button isn't shown.

=item GUI:otherprimaries

  EG:  ['GUI:otherprimaries', primaryname => 
                              { title => '...', questions => [...] }]

Defines other primaries to be added to the QWizard primary set.

=item GUI:submodules

  EG:  ['GUI:submodules', 'primaryname']

Defines a list of other primaries that should be called after the
initial one.

=item GUI:post_answers

  EG:  ['GUI:post_answers', sub { do_something(); }]

Defines an option for QWizard post_answers subroutines to run.

=item GUI:actions

  EG:  ['GUI:actions', sub { do_something(); }]

Defines an option for QWizard actions subroutines to run.

=item GUI:hook_finished

  EG:  ['GUI:hook_finished', sub { do_something(); }]

Defines subroutine(s) to be called after the GUI has completely finished.

=item GUI:VERSION

If display_help is defined, and the above token is specified
Getopt::GUI::Long takes over the output for --version output as well.

  EG:  ['GUI:VERSION','0.9']

Produces the --version help option:

  Help Options:
     -h                    Display help options -- short flags preferred
    --help                 Display help options -- long flags preferred
    --help-full            Display all help options -- short and long
    --version              Display the version number

And also auto-handles the --version switch:

  % PROGRAM --version
  Version: 0.9

=back

=head1 CONFIGURATION

If you call Getopt::GUI::Long's Configure routine, it will accept a
number of configure tokens and will pass the remaining ones to the
Getopt::Long Configure routine.  The tokens that it recognizes itself
are described below:

=over

=item display_help

The Getopt::GUI::Long package will auto-display help messages based on
the text included in the GetOptions call.  No more writing those silly
usage() functions!

Note that this differs from the Getopt::Long's implementation of the
auto_help token in that the information is pulled from the extended
GetOptions called instead of the pod documentation.

The display_help token will automatically add the following options to
the options the application will accept, and will catch and process
them as well.

=over

=item -h

Command line help preferring short options if present in the help
specification.

=item --help

Command line help preferring long options if present in the help
specification.

=item --help-full

Shows all available option names for a given option

=back

=item capture_output

This tells the Getopt::GUI::Long module that it should caputure the
resulting STDOUT and STDERR results from the script and display the
results in a window once the script has finished.

=back

=head1 PORTABILITY

If programs desire to not require this module, the following code
snippit can be used instead which will not fail even if this module is
not available.  To be used this way, the LocalGetOptions and
LocalOptionsMap functions should be copied to your perl script.

  LocalGetOptions(\%opts,
	          ["h|help", "Show help for command line options"],
	          ["some-flag=s", "perform some flag based on a value"]);

  sub LocalGetOptions {
      if (eval {require Getopt::GUI::Long;}) {
  	import Getopt::GUI::Long;
        # optional configure call
	Getopt::GUI::Long::Configure(qw(display_help no_ignore_case capture_output));
  	return GetOptions(@_);
      }
      require Getopt::Long;
      import Getopt::Long;
      # optional configure call
      Getopt::Long::Configure(qw(auto_help no_ignore_case));
      GetOptions(LocalOptionsMap(@_));
  }

  sub LocalOptionsMap {
      my ($st, $cb, @opts) = ((ref($_[0]) eq 'HASH') 
  			    ? (1, 1, $_[0]) : (0, 2));
      for (my $i = $st; $i <= $#_; $i += $cb) {
  	if ($_[$i]) {
	    next if (ref($_[$i]) eq 'ARRAY' && $_[$i][0] =~ /^GUI:/);
  	    push @opts, ((ref($_[$i]) eq 'ARRAY') ? $_[$i][0] : $_[$i]);
  	    push @opts, $_[$i+1] if ($cb == 2);
  	}
      }
      return @opts;
  }

=head1 EXAMPLES

See the getopttest program in the examples directory for an exmaple
script that uses a lot of these features.

=head1 AUTHOR

Wes Hardaker, hardaker@users.sourceforge.net

=head1 SEE ALSO

perl(1)

modules: QWizard

=cut

1;
