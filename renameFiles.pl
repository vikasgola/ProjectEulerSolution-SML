#!/usr/bin/perl

=begin comment
==========Simply assignment statement and print==========
# $a = 1;
# print "$a\n"

==========Replacing occurences of cat with dog ==========
# $string = "The cat sat on the mat";
# $string =~ s/cat/dog/;
# 
# print "$string\n";


==========A simple for loop to understand loops==========
# $num = 11;
# 
# for($i = 1; $i <= $num ; $i ++){
#     print "$i ";
# }
# print "\n";
=cut #end of comments

# get name of all files in the current directory
opendir my $dir, "." or die "Cannot open directory: $!";
my @files = readdir $dir;
closedir $dir;

foreach $file (@files){
    # check whether file follows the standard "Problem (ProblemNum).sml"
    if($file =~ /Problem ([1-9][0-9]*).sml/){
        $num = $1;
        # rename to the new format "Problem_(ProblemNum).sml"
        # rename("$file", "Problem $num.sml");
        
        # need to do git mv instead of normal rename
        qx(git mv Problem\\ $num.sml Problem_$num.sml);
    }
}