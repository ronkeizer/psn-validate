## PsN validation script
## 2012, Ron Keizer

use strict;
use File::Copy;
use File::Path;
use File::Basename;
use Cwd;

our $base = &File::Basename::dirname(Cwd::realpath($0));
do ($base."/functions.pl");
my @valid_commands = ("execute", "bootstrap", "vpc", "llp", "scm", "cdd", "lasso");

my $help_h ="\n  valpsn\n\n".
    "    Running validation test on PsN toolkit commands.".
    "\n\n".
    "        [ -h | -? ]\n".
    "        [ --help ]\n".
    "        [ --ini=<ini-file> ]\n".
    "        [ --report=<report-file> ]\n".
    "        [ --force_run_psn=0|1 ]\n".
    "        [ --force_run_test=0|1 ]\n".
    "        [ --force=0|1 ]\n".
    "        [ --run_only=1,2,3,...]\n\n";

my $help_help = "\n  valpsn\n\n".
    "    Running validation test on PsN toolkit commands.\n\n\n\n\n".
    "  Description:\n\n".
    "    The valpsn utility is a Perl script that allows you to perform\n".
    "    a list of validation tests on PsN toolkit commands.\n".
    "    To run the tool, it is necessary to create an ini-file which defines\n".
    "    the tests to be run. Examples are included with this release.\n\n".
    "  Examples:\n\n".
    "    \$ valpsn --ini=ref_20120930.ini\n\n".
    "    Runs the validation procedure defined in the ini-file.\n\n".
    "    \$ valpsn --ini=ref_20120930.ini --no_run --report=val_20120930.txt\n\n".
    "    Runs the validation procedure defined in the ini-file, but does not run the\n".
    "    PsN tool itself: it assumes the runs are already performed and output is\n".
    "    available. It does perform the validation tests defined in the R-scripts\n".
    "    and generates a report.\n".
    "\n".
    "  Options:\n\n".
    "    The options are given here in their long form. Options may not be abbre-\n".
    "    viated.\n\n".
    "    -ini=<ini-file> \n\n".
    "    The configuration file for the validation procedure. See manual for more\n".
    "    information on the syntax of this file. Examples are available in the \n".
    "    /examples folder.".
    "\n\n".
    "    -report=<report-file> \n\n".
    "    The name of the report file to generate, overrides the ini-file.".
    "\n\n".
    "    -force_run_psn=0|1 \n\n".
    "    Overrides the 'run_psn'-settings in the ini-file.".
    "\n\n".
    "    -force_test_psn=0|1 \n\n".
    "    Overrides the 'run_test'-settings in the ini-file.".
   "\n\n".
    "    -force=0|1 \n\n".
    "    Overrides the protection against overwriting a previous valiation report.".
    "\n\n".
    "    -run_only=1,2,3,... \n\n".
    "    A comma-separated list of test indices to run. This option is e.g. useful\n".
    "    if in a specific validation, only one or a few tests failed. In such cases\n".
    "    this argument can be used to only rerun the failed tests. Make sure to \n".
    "    specify a new report file though.".
    "\n\n";

if ($^O ne "linux") {
    if ($^O eq "darwin") {
#	print ("\n*** Warning: This validation tool is intended to run on Linux, its performance\non OSX has not been evaluated (but it should run fine anyhow).\n\n");
    } else {
	print ("\n*** Error: This validation tool is intended to run on Linux, ".$^O." is cur-\nrently not supported.\n\n");
	exit;
    }
}

## Read in the command line arguments
my %args;
my @possible_args = ("ini", "force_run_psn", "force_run_test", "report",
		     "h", "help", "force", "run_only");
my @not_recognized;
foreach my $arg (@ARGV) {
    if (substr($arg, 0, 1) eq "-") {
	my ($key, $value) = split("=", $arg);
	$key =~ s/\-//g;
	$value =~ s/[\"\']//g;
	if (exists_in_array($key, \@possible_args)) {
	    if ($key ne "") {
		$args{$key} = $value;
	    }
	} else {
	    push (@not_recognized, $key);
	}
    }
}
my @recognized_args = keys (%args);
my @run_only;
my $run_only_activated = 0;
if ($args{run_only} ne "") {
    @run_only = split(',', $args{run_only});
    $run_only_activated = 1;
}
if (exists_in_array("h", \@recognized_args)) {
    print $help_h;
    exit;
}
if (exists_in_array("help", \@recognized_args)) {
    print $help_help;
    exit;
}

if (int(@not_recognized) > 0) {
    print "\n*** Error: one or more of the specified argument(s) were not recognized: ".join (" ", @not_recognized).".\n";
    print "Possible arguments are: -".join (" -", @possible_args)."\n\n";
    exit;
}

unless (-e $args{ini}) {
    print "\nPsN validate command requires an ini-file to run. See PsN website for more details\n\n".
	"Usage e.g.: valpsn -ini=val_ex_1.ini\n\n";
    print "Possible arguments are: ".join (" ", @possible_args)."\n\n";
    exit;
}

## Read the ini-file
open (INI, "<".$args{ini});
my @lines = <INI>;
my $ini_area;
my %empty;
my @empty_order;
my @ini_areas_order;
my @current_order;

my %general;
my %folders;
my %software;
my %validation;

my %current;
my @psn_commands;
my @ini_areas;
my $i = 1;
my $cnt = 1;
foreach my $line (@lines) {
    $line = rm_spaces($line);
#    if (substr($line, 0, 1) ne "#") {
	my ($code, $comments) = split ("#", $line);
	chomp ($comments);
#	unless ($code eq "") {
	    if ((substr($code,0,1) eq "[")||($i == int(@lines))) {
		if ($cnt > 1) {
		    if ($ini_area =~ m/(folders|software|validation)/) {
			if ($ini_area eq "folders") {
			    %folders = %current;
			}
			if ($ini_area eq "software") {
			    %software = %current;
			}
			if ($ini_area eq "validation") {
			    %validation = %current;
			}
		    } else {
			my %save = %current;
			my @save_order = @current_order;
			push (@ini_areas, \%save);
			push (@ini_areas_order, \@save_order);
		    }
		}
		$cnt++;
		%current = %empty;
		@current_order = @empty_order;
		my $tmp = $code;
		$tmp =~ m/\[(.*?)\]/;
		$ini_area = $1;
		unless ($ini_area =~ m/(folders|software|validation)/) {
		    push (@psn_commands, $ini_area);
		}
	    }
	    unless ((substr($code,0,1) eq "\[")||($code eq "")) {
		my ($key, @rest) = split("=", $code);
		my $value = join ("=", @rest);
		$current{rm_spaces($key)} = rm_spaces($value);
		push (@current_order, rm_spaces($key));
	    }
#	}
 #   }
    $i++;
}

close INI;

## check ini-file keys
my @allowed_keys = ("run_folder", "report", "perl_executable", "psn_executables", "psn_version", "R_executable",
		    "nm_version", "run_psn", "run_test", "verbose_level", "extra_arguments", "tolerance", "ofv_abs_tol",
		    "nm_mod_extension", "nm_out_extension", "out_folder", "report_folder", "lib_folder",
		    "R_script_folder", "threads", "clean");
my @key_not_recognized;
foreach (keys(%validation)) { $general{$_} = $validation{$_} };
foreach (keys(%software)) { $general{$_} = $software{$_} };
foreach (keys(%folders)) { $general{$_} = $folders{$_} };
foreach (keys(%general)) {
    unless (exists_in_array($_, \@allowed_keys)) {
	push (@key_not_recognized, $_);
    }
}

if (@key_not_recognized > 0) {
    print "\n*** Error: one or more of the specified settings in the configuration file\nwere not recognized: ".join (" ", @key_not_recognized).".\n\n";
    print "Allowed general settings are: ".join (" ", @allowed_keys)."\n\n";
    exit;
}

## do we need to rerun? (from ini-file)
if ($general{run_psn} ne "") {
    if ($general{run_psn} =~ m/(true|1)/i) {
	$general{run_psn} = 1;
    }
    if ($general{run_psn} =~ m/(false|0)/i) {
	$general{run_psn} = 0;
    }
} else {
    $general{run_psn} = 1;
}
## do we need to rerun? (from command line)
my @specified_args = keys(%args);
if (exists_in_array("no_run", \@specified_args)) {
    $general{run_psn} = 0;
}
## Do we need to invoke R to perform the actual validation?
my $test = 1;
if ($general{run_test} ne "") {
    if ($general{run_test} =~ m/(true|1)/i) {
	$general{run_test} = 1;
    }
    if ($general{run_test} =~ m/(false|0)/i) {
	$general{run_test} = 0;
    }
}

## Create folders for output if not existing yet
my $home = $ENV{HOME};
my @folders = ("lib_folder", "R_script_folder", "out_folder", "report_folder",
	       "run_folder");
foreach my $key (@folders) {
    $general{$key} =~ s/\~/$home/g;
    $general{$key} =~ s/\%BASE/$base/g;
    $general{$key} =~ s/\%HOME/$home/g;
    $general{$key} =~ s/\$BASE/$base/g;
    $general{$key} =~ s/\$HOME/$home/g;
}
create_folder_recursively ($general{out_folder}."/".$general{run_folder});
create_folder_recursively ($general{report_folder});
pop (@folders);
foreach my $key (@folders) {
    unless (-d $general{$key}) {
	print ("*** Error: specified ".$key." (".$general{$key}.") was not found!. Please check your configuration file. Stopping this validation.\n\n");
	exit;
    }
}

## Check the report file
my $report = $general{report_folder}."/".$general{report};
if (exists_in_array("report", \@specified_args)) {
    $report = $args{report};
}
if (-e $report) {
    unless ($args{force}) {
	print "\n*** Error: Validation report file already exists. This tool does not allow over-\nwriting. ".
	    "Please specify a different filename and try again. Alternatively,\n".
	    "override with the --force=1 argument (will overwrite the old validation report).\n\n";
	exit;
    }
}

## Log all screen output to file
open (STDOUT, "| tee -ai ".$report);

## Start
print "\n------------------------------------------------------------------------------\n";
print "PsN validation tool\n";
print "2012-2013, Pirana Software & Consulting BV (RJK)\n";
print "------------------------------------------------------------------------------\n\n";

## Capture the PsN version information and available NM versions
my $psn;
my $cwd = fastgetcwd();
if ($general{psn_executables} ne "") {
    $psn = unix_path($general{psn_executables}."/");
}
my $psn_version_add; ## add a version number to the psn commands?
if ($general{psn_version} ne "") {
    $psn_version_add = "-".$general{psn_version};
}
system ($psn."psn".$psn_version_add." -version");
print "\nNONMEM versions installed:";
system ($psn."psn".$psn_version_add." -nm_versions");

my @R_scripts = dir($cwd."/R", '.R');
print "Starting validation tests.\n";
my $i = 0;
my $success = 0;
my $failed = 0;
my $not_tested = 0;
my @failed_tests;
my @get_from_general = ("nm_version", "run_psn","run_test", "extra_arguments", "tolerance", "ofv_abs_tol",
			"nm_out_extension", "nm_mod_extension", "verbose_level", "threads", "clean");
print @run_only;
foreach my $hash (@ini_areas) {
    my $psntool = shift (@psn_commands);
    my $hash_order = shift (@ini_areas_order);
    if (exists_in_array($psntool, \@valid_commands)) {
	$i++;
    	print "\n-------------------------------------------------------------------------------\n";
	print "Test ".$i.": ".$psntool."\n";
	print "-------------------------------------------------------------------------------\n\n";
	my $run_this = 1;
	if ($run_only_activated) {
	    $run_this = 0;
	    if (exists_in_array($i, \@run_only)) {
		$run_this = 1;
	    }
	}
	if ($run_this) {
	    ## Copy some arguments from the general hash, if not specified
	    my %current = %$hash;
	    foreach my $arg (@get_from_general) {
		unless (exists($current{$arg})) {
		    $current{$arg} = $general{$arg};
		}
	    }
	    if (exists($args{force_run_test})) {
		$current{run_test} = $args{force_run_test};
	    }
	    if (exists($args{force_run_psn})) {
		$current{run_psn} = $args{force_run_psn};
	    }
	    my $R_script = $general{R_script_folder}."/".$psntool.".R";
	    if (exists($current{R_script})) {
		$R_script = $general{R_script_folder}."/".$current{R_script};
	    }
	    unless (-e $R_script) {
		print "*** Error: validation R-script for this test not found!!.\nStopping this test.\n\n";
		push (@failed_tests, $i.": ".$psntool. " (validation R-script not found)");
		$failed++;
		next;
	    }
	    if ($current{run_test} =~ m/(0|false)/i) {
		$not_tested++;
	    }
	    #	$current{reference_copy} = 'reference.'.$ext;
	    if ($current{verbose_level} > 1) {
		print "Validation settings:\n";
		print hash_print(\%current, $hash_order)."\n";
	    }

	    ## check if reference and output file specified, otherwise use default
	    my $mod = $current{model};
	    my @m = split (/\./, $mod);
	    pop(@m);
	    my $mod_no_ext = join ('.', @m);
	    if ($current{reference} eq "") {
		$current{reference} = $mod_no_ext.".".$current{nm_out_extension};
	    }
#	    $current{nm_output} = $psntool."_test_dir/NM_run1/psn.".$current{nm_out_extension};
	    $current{nm_output} = $mod_no_ext.".".$current{nm_out_extension};
#	}

	    # Copy files
	    my $run_folder = $general{out_folder}."/".$general{run_folder}."/test".$i."_".$psntool;
	    mkdir ($run_folder);

	    print $i."a. Copying library files to run folder (".$run_folder.")\n\n";
	    if (-e $general{lib_folder}."/".$current{folder}."/".$current{model}) {
		copy ($general{lib_folder}."/".$current{folder}."/".$current{model},
		      $run_folder."/".$current{model});
	    } else {
		print "*** Error: model file (".$general{lib_folder}."/".$current{folder}."/".$current{model}.") not found!!!\nStopping this test.\n\n";
		push (@failed_tests, $i.": ".$psntool. " (model not found)");
		$failed++;
		next;
	    }

	    ## Copy reference files (lst for execute, and lst+raw_results+parsed results for other tools)
	    ## Always try to copy lst-file: this is needed for e.g. bootstrap/vpc to ensure the seed option is working
	    my $lst_found = 0;
	    if ($psntool eq "execute") {
		if (-e $general{lib_folder}."/".$current{folder}."/".$current{reference}) {
		    copy ($general{lib_folder}."/".$current{folder}."/".$current{reference}, $run_folder."/".$current{reference}.".ref");
		    $lst_found = 1;
		}
		if ($lst_found == 0) {
		    print "*** Error: reference output file (".$current{reference}.") not found!!!\nStopping this test.\n\n";
		    push (@failed_tests, $i.": ".$psntool. " (reference output not found)");
		    $failed++;
		    next;
		}
	    } else {
		if ($current{lst} ne "") {
		    if (-e $general{lib_folder}."/".$current{folder}."/".$current{lst}) {
			copy ($general{lib_folder}."/".$current{folder}."/".$current{lst}, $run_folder."/".$current{lst});
			$lst_found = 1;
		    }
		    if ($lst_found == 0) {
			print "*** Error: specified lst-file (".$current{reference}.") not found!!!\nStopping this test.\n\n";
			push (@failed_tests, $i.": ".$psntool. " (lst-file not found)");
			$failed++;
			next;
		    }
		}
		if (($lst_found == 0)&&($current{seed} ne "")) {
		    print "*** Warning: The -seed option was specified, but the lst-file was not found.\n".
			"This could potentially invalidate the -seed option, e.g. for bootstrap / vpc etc.\n\n";
		}
	    }
	    unless($psntool eq "execute") {
		my @csv;
		my $dir_get = 'raw_results(.)*.csv';
		unless (exists_in_array($psntool, ["lasso","scm"])) {
		    if (-e $general{lib_folder}."/".$current{folder}."/".$current{reference}) {
			@csv = dir ($general{lib_folder}."/".$current{folder}."/".$current{reference}, $dir_get);
		    }
		    if (@csv > 0) {
			my $raw_res = shift(@csv);
			copy ($general{lib_folder}."/".$current{folder}."/".$current{reference}."/".$raw_res, $run_folder."/raw_results_ref.csv");
		    } else {
			print "*** Error: reference output file (".$current{reference}.") not found!!!\nStopping this test.\n\n";
			push (@failed_tests, $i.": ".$psntool. " (reference output not found)");
			$failed++;
			next;
		    }
		    if (-e $general{lib_folder}."/".$current{folder}."/".$current{reference}."/".$psntool."_results.csv") {
				copy ($general{lib_folder}."/".$current{folder}."/".$current{reference}."/".$psntool."_results.csv",
			      $run_folder."/".$psntool."_results_ref.csv");
		    } else {
				print "*** Error: reference results file (".$current{reference}."/".$psntool."_results.csv) not found!!!\nStopping this test.\n\n";
		    	$failed++;
				next;
		    }
		}
		if ($psntool eq "scm") {
		    if (-e $general{lib_folder}."/".$current{folder}."/".$current{reference}."/short_scmlog.txt") {
			copy ($general{lib_folder}."/".$current{folder}."/".$current{reference}."/short_scmlog.txt",
			      $run_folder."/short_scmlog_ref.txt");
		    } else {
			push (@failed_tests, $i.": ".$psntool. " (Reference short_scmlog.txt not found)");
			$failed++;
			next;
		    }
		}
		if ($psntool eq "lasso") {
		    if (-e $general{lib_folder}."/".$current{folder}."/".$current{reference}."/coeff_table.log") {
			copy ($general{lib_folder}."/".$current{folder}."/".$current{reference}."/coeff_table.log",
			      $run_folder."/coeff_table_ref.log");
		    } else {
			push (@failed_tests, $i.": ".$psntool. " (Reference coeff_table.log not found)");
			$failed++;
			next;
		    }
		}
	    }

	    if ($current{extra_files} ne "") {
		my @files = split(",", $current{extra_files});
		foreach my $file(@files) {
		    $file = rm_spaces($file);
		    if (-e $general{lib_folder}."/".$current{folder}."/".$file) {
			copy ($general{lib_folder}."/".$current{folder}."/".$file, $run_folder."/".$file);
		    } else {
			print "*** Error: raw_results file not found!!!\nStopping this test.\n\n";
			push (@failed_tests, $i.": ".$psntool. " (raw_results file not found)");
			$failed++;
			next;
		    }
		}
	    }

	    ## get dataset
	    my $mod = extract_from_model($general{lib_folder}."/".$current{folder}."/".$current{model});
	    my $dataset = $$mod{dataset};
	    if (-e $general{lib_folder}."/".$current{folder}."/".$dataset) {
		copy ($general{lib_folder}."/".$current{folder}."/".$dataset, $run_folder."/".$dataset);
	    } else {
		print "*** Error: Required dataset not found!!!\nStopping this test.\n\n";
		push (@failed_tests, $i.": ".$psntool. " (dataset not found)");
		$failed++;
		next;
	    }

	    chdir ($cwd."/".$run_folder);
	    # Start command
	    my $comm = $psntool." ";
	    $comm .= $current{extra_arguments}." ";
	    # handle NM version; # implement feature to specify multiple nm_versions in later version?
	    $comm .= "-dir=".$psntool."_test_dir ";
	    if ($current{nm_version} ne "") {
		$comm .= "-nm_version=".$current{nm_version}." ";
	    }
	    if ($current{threads} ne "") {
		$comm .= "-threads=".$current{threads}." ";
	    }
	    if ($current{clean} ne "") {
		$comm .= "-clean=".$current{clean}." ";
	    }
	    unless (exists_in_array($psntool, ["bootstrap"])) {
		    if ($current{lst} ne "") {
				$comm .= "-lst=".$current{lst}." ";
	    	}
		    if ($current{lst_file} ne "") {
				$comm .= "-lst_file=".$current{lst_file}." ";
	    	}
	    }
	    if ($current{seed} ne "") {
		$comm .= "-seed=".$current{seed}." ";
	    }
	    if (exists_in_array($psntool, ["bootstrap","npc","vpc","mcmp","sse"])) {
		if ($current{samples} ne "") {
		    $comm .= "-samples=".$current{samples}." ";
		} else {
		    print "*** Error: -samples argument not supplied to ".$psntool."!!!\nStopping this test.\n\n";
		    push (@failed_tests, $i.": ".$psntool. " (-samples argument not supplied)");
		}
	    }
	    if ($psntool eq "cdd") {
		if ($current{case_column} ne "") {
		    $comm .= "-case_column=".$current{case_column}." ";
		} else {
		    print "*** Error: -case_column argument not supplied to ".$psntool."!!!\nStopping this test.\n\n";
		    push (@failed_tests, $i.": ".$psntool. " (-case_column argument not supplied)");
		}
	    }
	    if ($psntool eq "lasso") {
		if ($current{relations} ne "") {
		    $comm .= "-relations=".$current{relations}." ";
		} else {
		    print "*** Error: -relations argument not supplied to ".$psntool."!!!\nStopping this test.\n\n";
		    push (@failed_tests, $i.": ".$psntool. " (-relations argument not supplied)");
		}
	    }
	    # update command;
	    if ($current{command_explicit} ne "") {
		$comm = $current{command_explicit};
	    }
	    my $run = 1;
	    if ($general{run_psn} ne "") {
		if ($general{run_psn} =~ m/(true|1)/i) {
		    $run = 1;
		}
		if ($general{run_psn} =~ m/(false|0)/i) {
		    $run = 0;
		}
	    }
	    if ($current{run_psn} ne "") {
		if ($current{run_psn} =~ m/(true|1)/i) {
		    $run = 1;
		}
		if ($current{run_psn} =~ m/(false|0)/i) {
		    $run = 0;
		}
	    }
	    if ($psntool eq "scm") {
		unless ($current{model} eq "") {
		    $comm .= "-model=".$current{model};
		}
	    } else {
		$comm .= $current{model};
	    }
	    if ($current{verbose_level} < 3) {
		$comm .= " > run.log";
	    }
	    if ($run) {
		print $i."b. Running command: '".$comm."'\n\n";
		chdir($run_folder);
		rmtree ($psntool."_test_dir");
		system ($comm);
		chdir($cwd);
	    }

	    ## Run sumo on output if it was an execute command
	    if ($psntool eq "execute") {
		chdir($run_folder);
		my $comm = "sumo ".$current{nm_output};
		if ($current{verbose_level} == 2) {
		    print "\Test sumo output: '".$comm."'\n\n";
		    system ($comm);
		}
		print "\nSaving sumo output to ".$current{nm_output}.".csv \n";
		my $comm = "sumo -csv ".$current{nm_output}." > ".$current{nm_output}.".csv";
		system ($comm);

		## reference
		my $comm = "sumo ".$current{reference}.".ref";
		if ($current{verbose_level} == 2) {
		    print "\nReference sumo output: '".$comm."'\n\n";
		    system ($comm);
		}
		print "\nSaving reference sumo output to ".$current{reference}."_ref.csv \n";
		my $comm = "sumo -csv ".$current{reference}.".ref > ".$current{reference}."_ref.csv";
		system ($comm);
		chdir($cwd);
	    }

	    my $test = $general{run_test};
	    if ($current{run_test} ne "") {
		if ($current{run_test} =~ m/(true|1)/i) {
		    $test = 1;
		}
		if ($current{run_test} =~ m/(false|0)/i) {
		    $test = 0;
		}
	    }

	    if ($test) {
		chdir($run_folder);
		## test if PsN output folder present
		unless (-d $psntool."_test_dir") {
		    print "*** Error: PsN output folder not found!!!\nStopping this test.\n\n";
		    push (@failed_tests, $i.": ".$psntool. " (PsN output folder not found)");
		    $failed++;
		    next;
		}
		# Compare output
		print $i."c. Comparing output\n\n";
		my $hash_list = "args <- list (\n";
		my $j = 1;
		my @keys = sort {lc($a) cmp lc($b)} keys(%current);
		foreach my $key (@keys) {
		    $hash_list .= "  ".$key.' = "'.$current{$key}.'"';
		    unless ($j == int(keys(%current))) {
			$hash_list .= ","
		    }
		    $hash_list .= "\n";
		    $j++;
		}
		$hash_list .= ")\n\n";
		my $R_code = file_to_text($R_script);
		open (RCOPY, ">".$run_folder."/test".$i."_".$psntool.".R");
		print RCOPY 'cwd <- "'.$run_folder.'"'."\n";
		print RCOPY 'setwd(cwd)'."\n";
		print RCOPY $hash_list;
		print RCOPY $$R_code;
		print RCOPY 'cat(paste("Log file: ", cwd, "/test'.$i.'_'.$psntool.'.log\n", sep=""))'."\n";
		print RCOPY "quit()\n";
		close RCOPY;
		chdir($run_folder);
		my $R_command = "cat test".$i."_".$psntool.".R | ".$general{R_executable}." --no-save > test".$i."_".$psntool.".log";
		print "Running R-script: test".$i."_".$psntool.".R \n";
		open (R, "$R_command 2>&1 |"); # redirect STDERR to STDOUT
		system ($R_command);
		my $R_log = file_to_text("test".$i."_".$psntool.".log");
		my @lines = split("\n", $$R_log);
		my $curr_success = 0;
		print "Output from R: \n";
		foreach my $line (@lines) {
		    if (substr($line,0,4) =~ m/test/i) {
			print "  ". $line."\n";
		    }
		    if (substr($line, 0, 9) eq "Log file:") {
			print "\n". $line."\n";
		    }
		    if ((substr($line,0,12) =~ m/OVERALL TEST/ig)&&($line =~ m/SUCCES/ig)) {
			print "  ". $line."\n";
			$curr_success = 1;
		    }
		}
		close R;
		if ($curr_success == 1) {
		    $success++;
		} else {
		    push (@failed_tests, $i.": ".$psntool." (comparison with reference failed)");
		    $failed++;
		}
		print "\n";
		# return to main folder
		chdir ($cwd);
	    }
	}
	else {
	    print "This test was skipped.\n\n";
	}
    }
}

print "\nValidation tests are done.\n\n";
print "-------------------------------------------------------------------------------\n";
print "Summary: Succes (".$success."/".$i."), failed (".$failed."/".$i."), not tested (".$not_tested."/".$i.")\n";
if ($failed > 0) {
    print "Failed tests: ";
    my $i = 1;
    foreach my $test (@failed_tests) {
	if ($i == 1) {
	    print $test."\n";
	} else {
	    print "              ".$test."\n";
	}
	$i++;
    }
}
print "-------------------------------------------------------------------------------\n\n";

close (STDOUT);
