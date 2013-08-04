sub exists_in_array {
    my  ($key, $array_ref ) = @_ ;
    my $match = 0;
    foreach (@$array_ref) {
	if ($key eq $_) {
	    $match = 1;
	}
    }
    return($match);
}

sub rm_spaces {
# Remove leading and trailing spaces and \n
    my $str = shift;
    chomp ($str);
    $str =~ s/^\s+//; # leading spaces
    $str =~ s/\s+$//; # trailing space
    return ($str);
}

sub hash_print {
### Purpose : For debugging, print hash keys and values.
### Compat  : W+L+
    my ($hash_ref, $order_ref) = @_;
    my %hash = %$hash_ref;
    my $text;
    if ($order_ref =~ m/ARRAY/) {
	foreach my $key (@$order_ref) {
	    $text .= " * ". $key.": ".$hash{$key}."\n";
	}
    } else {
	foreach my $key (keys(%hash)) {
	    $text .= " * ". $key.": ".$hash{$key}."\n";
	}
    }
    return($text);
}

sub extract_from_model {
### (abridged version from Pirana module)
### Purpose : Extract information about the model from a NM model file
### Compat  : W+L+
  my $file = shift;
  open (CTL,"<".$file);
  my @ctl_lines=<CTL>;
  close CTL;
  my $description;
  my $descr_area = 0; my $author_area = 0;
  # search first few lines for reference model and model description.
  for (my $j =0; $j < 20; $j++) {
      if ((substr(@ctl_lines[$j],0,1) eq ";")&&((@ctl_lines[$j] =~ m/Ref/i)||(@ctl_lines[$j] =~ m/Parent/i)||(@ctl_lines[$j] =~ m/based on/i))&&($refmod eq "")&&!(@ctl_lines[$j] =~ m/mu-ref/i)&&($j < 6)) {  # Census uses 'Parent', but you can also use anything containing Ref
	  @ctl_lines[$j] =~ s/\=/\:/g;  # for people that code 'Ref=001'
	  @ctl_lines[$j] =~ s/\s/\:/;   #in between spaces
	  my @l = split (/\:/, @ctl_lines[$j]); # get last word (hopefully the ref model no#)
	  $refmod = pop(@l);
	  $refmod =~ s/^\s+//; #remove leading spaces
	  $refmod =~ s/\"+$//;  #remove trailing spaces
	  $refmod =~ s/[\r\n]//g;
	  $descr_area =0;
      }
      # PsN run specification format:
      if ((substr(@ctl_lines[$j], 0,2) eq ";;")&&(@ctl_lines[$j] =~ m/based on:/i)) {  # Census uses 'Parent', but you can also use anything containing Ref
	  @ctl_lines[$j] =~ s/;;//;
	  @ctl_lines[$j] =~ s/\=/:/g;  # for people that code 'Ref=001'
	  @ctl_lines[$j] =~ s/\s/:/;   #in between spaces
	  my @l = split (/\:/, @ctl_lines[$j]); # get last word (hopefully the ref model no#)
	  $refmod = @l[int(@l)-1];
	  $refmod =~ s/^\s+//; #remove leading spaces
	  $refmod =~ s/\"+$//;  #remove trailing spaces
	  $descr_area = 0;
      }
      if ((substr(@ctl_lines[$j], 0,2) eq ";;")&&(@ctl_lines[$j] =~ m/description:/i)) {
	  $descr_area = 1;
	  @ctl_lines[$j] =~ s/2\.//;
      }
      if ((substr(@ctl_lines[$j], 0,2) eq ";;")&&(@ctl_lines[$j] =~ m/author:/i)) {
	  $descr_area = 0;
	  $mod{author} = @ctl_lines[$j];
	  chomp($mod{author});
	  $mod{author} =~ s/;;//g;
	  $mod{author} =~ s/x1.//gi;
	  $mod{author} =~ s/author://gi;
	  $mod{author} =~ s/^\s+//; #remove leading spaces
      }
      if ((substr(@ctl_lines[$j],0,1) eq ";")&&(@ctl_lines[$j] =~ m/model desc/i)) { # no run record used, take description from \$PROB
	  $description = @ctl_lines[$j];
	  $description =~ s/;//i;
	  $description =~ s/model//i;
	  $description =~ s/desc[^\s\:\=]*[\:\=]//i;
	  $description = rm_spaces($description);
      }
      if ((substr(@ctl_lines[$j],0,5) eq "\$PROB")&&($description eq "")) { # no run record used, take description from \$PROB
	  $description = @ctl_lines[$j];
	  $description =~ s/\$PROBLEM//i;
	  $description =~ s/\$PROB//i;
	  $description =~ s/\n//;
      }
      if ($descr_area == 1) {
	  if ((@ctl_lines[$j] =~ m/;;/)&&(@ctl_lines[$j] =~ m/(based on:|label:|structural model:|covariate model:|inter-individual variability:|inter-occasion variability:|residual variability:|estimation:)/i)) {
	      $descr_area = 0;
	  }
	  unless (@ctl_lines[$j] =~ m/;;/) {
	      $descr_area = 0;
	  }
	  @ctl_lines[$j] =~ s/;;//g;
	  if (@ctl_lines[$j] =~ m/description/i) {
	      @ctl_lines[$j] =~ s/description=//i;
	      @ctl_lines[$j] =~ s/description://i;
	  }
	  if ($descr_area == 1) {
	      $description .= @ctl_lines[$j];
	  }
      }
  }
  $description =~ s/^\s+//; #remove leading spaces
  # loop through model file to extract parameter names
  my $theta_area=0; my $omega_area=0; my $sigma_area=0; my $prior=0;
  my $theta_area_prv=0; my $omega_area_prv=0; my $sigma_area_prv=0; # needed to determine whether in Prior region or not
  my $table_area=0; my $estim_area=0; my $msf_file="";
  my $input_area=0; my @input; my $data_area; my @data_ignore; my @data_accept;
  my $cnt = 0;
  my @th_descr; my @om_descr; my @si_descr;
  my $om_comment_flag; my $si_comment_flag;
  my @tab_files;
  my (@th_fix, @om_fix, @si_fix);
  my @om_same; my $sigma_flag; my $method_added = 0;
  my @block = (1); my $last_om = 1;
  my %prior_rec; my @in_block; my $block_size;
  my @om_block_struct; my @si_block_struct;
  my @om_block_struct_fix; my @si_block_struct_fix;
  my @om_struct; my @si_struct; my $prv_line_was_block_fix; my $same_block_fix; my $om_same_flag;
  foreach (@ctl_lines) {
      $_ =~ s/^\s+//; #remove leading spaces
      if (substr($_,0,1) eq "\$") {
	  if ($theta_area==1) {$theta_area=0} ;
	  if ($omega_area==1) {$omega_area=0} ;
	  if ($sigma_area==1) {$sigma_area=0} ;
	  if ($table_area==1) {$table_area=0} ;
	  if ($estim_area==1) {$estim_area=0} ;
	  if ($data_area==1)  {$data_area=0; }
#				   print "ign: ".join(" ", @data_ignore)."\n" ;
#				   print "acc: ".join(" ", @data_accept)."\n"} ;
      }
      if ((substr ($_,0,1) eq ";")&!(($_ =~ m/model desc/i)||($_ =~ m/ref\. model\:/i)||(substr($_,0,2) eq ";;"))) {
	  my $comment = $_;
	  $comment =~ s/;//g;
	  $comment = rm_spaces($comment);
	  push (@comments, $comment);
      } else {
	  if (substr ($_,0,6) eq "\$THETA") {$theta_area = 1; $theta_area_prv=1; }
	  if (substr ($_,0,6) eq "\$OMEGA") {$omega_area = 1; if ($theta_area_prv==1) {$omega_area_prv=1;} }
	  if (substr ($_,0,6) eq "\$SIGMA") {$sigma_area = 1; $sigma_flag = 1; }
	  if (substr ($_,0,4) eq "\$TAB") {$table_area = 1 };
	  if (substr ($_,0,4) eq "\$EST")   {$estim_area = 1 };
	  if (substr ($_,0,6) eq "\$INPUT") {$input_area = 1 };
	  if (substr ($_,0,4) eq "\$SIM")   {
	      $mod{method} = add_item($mod{method},"SIM", $_); $method_added=1;
	  };
	  if (substr ($_,0,5) eq "\$DATA") {
	      $data_area = 1;
	      my @data_arr = split (" ", $_);
	      shift(@data_arr);
	      my $dataset = "";
	      while (($dataset eq "")&&(@data_arr>0)) {$dataset = shift(@data_arr)};
	      $mod{dataset} = $dataset;
	  }
      }
  }
  $mod{description} = $description;
  $mod{refmod} = $refmod;
  $mod{refmod} =~ s/\s//g;
  $mod{comment_lines} = \@comments;
  return (\%mod);
}

sub dir {
### Purpose : Return files in a dir
### Compat  : W+L+
  my ($dir, $filter) = @_;
  undef my @dirfiles;
  my @dirread;
  if (-d $dir) {
      if ($dir eq "//") { $dir = "." }
      opendir ( DIR, $dir) || die "Error in opening dir $dir\n";
      while (my $filename = readdir(DIR)) {
	  push (@dirread, $filename);
      };
      closedir(DIR);
      foreach my $filename (@dirread) {
	  my $l = length($filename);
	  if (($filename =~ m/$filter/i)||($filter eq "")) {
	      push (@dirfiles, $filename);
	  }
      }
      sort (ascend @dirfiles);
  }
  if (@dirfiles[(int(@dirfiles)-1)] eq "") { pop @dirfiles }; # last value can be empty sometimes
  return @dirfiles;
}

sub text_to_file {
    my ($text_ref, $filename) = @_;
    if (open (TXT, ">".$filename)) {
	print TXT $$text_ref;
    };
    close (TXT);
}

sub file_to_text {
    my $filename = shift;
    my $text = "";
    if (-e $filename) {
	open (TXT, "<".$filename);
	my @lines = <TXT>;
	$text = join ("", @lines);
	close (TXT);
    }
    return (\$text);
}

sub extract_file_name {
### Purpose : Return only the filename from a full given path
### Compat  : W+L+
  my $full = unix_path(shift);
  my @parts = split ("/",$full);
  my $file_name = @parts[int(@parts)-1];
  return($file_name);
}

sub get_file_extension {
    my $filename = shift;
    my @spl = split (/\./, $filename);
    my $ext = "";
    if (@spl > 1) {
	$ext = @spl[(@spl-1)];
    }
    return($ext);
}

sub win_path {
### Purpose : Return a path with only \
### Compat  : W+L+
  my $win_dir = @_[0];
  $win_dir =~ s/\//\\/g ;
  $win_dir =~ s/\\\\/\\/g;  # if '\\' occurs then '\'
  return $win_dir;
}

sub unix_path {
### Purpose : Return a path with only /
### Compat  : W+L+
  my $unix_dir = @_[0];
  $unix_dir =~ s/\\/\//g ;
  $unix_dir =~ s/\/\//\//g; # remove double //
  return $unix_dir;
}

sub os_specific_path {
  my $str = shift;
  if ($^O =~ m/MSWin/i) {
    $str = win_path($str);
  } else {
    $str = unix_path($str);
  }
  return $str;
}

sub create_folder_recursively {
### Purpose : Create a folder given an absolute path, but first create the folders up from the specified folder if they don't exist.
### Compat  : W+L+
    my $folder = shift;
    my @subs = split ("/", unix_path($folder));
    my $curr;
    foreach (@subs) {
	$curr .= $_."/";
#	$curr = $_;
#	print "Creating folder: ".$curr."\n";
	unless (-d $curr) {
	    print "Creating folder: ".$curr."\n";
	    mkdir ($curr);
	}
    }
}
