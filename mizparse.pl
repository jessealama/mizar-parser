#!/usr/bin/perl

use strict;
use warnings;

use LWP;
use Getopt::Long qw(:config gnu_compat);
use Pod::Usage;
use Readonly;
use charnames qw(:full);
use English qw(-no_match_vars);
use version;
use Carp qw(croak);

Readonly my $VERSION => qv('1.1');

my $man       = 0;
my $help      = 0;
my $verbose   = 0;
my $debug     = 0;
my $transform = 'none';
my $format    = 'xml';
my $timeout   = '60';
my $version   = 0;

Readonly my $MIZAR_PARSER_BASE_URI => 'http://mizar.cs.ualberta.ca/parsing/';

Readonly my $FULL_STOP    => q{.};
Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES   => q{  };
Readonly my $DASH         => q{-};

sub ensure_valid_transform {
    return (   $transform eq 'none'
            || $transform eq 'wsm'
            || $transform eq 'msm' );
}

sub ensure_valid_format {
    return ( $format eq 'text' || $format eq 'xml' );
}

sub ensure_sensible_timeout {
    return ( $timeout > 0 );
}

sub process_commandline {

    GetOptions(
        'help|?'      => \$help,
        'man'         => \$man,
        'verbose'     => \$verbose,
        'debug'       => \$debug,
        'transform=s' => \$transform,
        'format=s'    => \$format,
        'timeout=i'   => \$timeout,
        'version'     => \$version,
    ) or pod2usage(2);

    if ($help) {
        pod2usage(1);
    }

    if ($man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2
        );
    }

    if ($version) {
        print $VERSION, "\N{LF}";
        exit 0;
    }

    if ( !ensure_valid_transform() ) {
        pod2usage(1);
    }

    if ( !ensure_valid_format() ) {
        pod2usage(1);
    }

    if ( scalar @ARGV != 1 ) {
        pod2usage(1);
    }

    if ($debug) {
        $verbose = 1;
    }

    if ( !ensure_sensible_timeout() ) {
        print {*STDERR} 'Error: \'', $timeout,
            '\' is not a sensible value for a timeout.', "\N{LF}";
        return 0;
    }

    return 1;

}

sub ensure_sensible_article {

    my $path = shift;

    if ( $path eq $DASH ) {
        return 1;
    }

    if ( !-e $path ) {
        print {*STDERR} 'Error: there is no file at the given path',
            "\N{LF}", "\N{LF}", $TWO_SPACES, $path, "\N{LF}",
            "\N{LF}";
        return 0;
    }

    if ( -d $path ) {
        print {*STDERR} 'Error: the supplied article', "\N{LF}",
            "\N{LF}", $TWO_SPACES, $path, "\N{LF}", "\N{LF}",
            'is not a file but a directory.';
        return 0;
    }

    if ( !-r $path ) {
        print {*STDERR} 'Error: the article file', "\N{LF}",
            "\N{LF}", $TWO_SPACES, $path, "\N{LF}", "\N{LF}",
            'is unreadable.';
        return 0;
    }

    return 1;

}

sub slurp {
    my $path_or_fh = shift;

    open (my $fh, '<', $path_or_fh)
	or die 'Error: unable to open the file (or filehandle) ', $path_or_fh, '.';

    my $contents;
    { local $/; $contents = <$fh>; }

    close $fh
	or die 'Error: unable to close the file (or filehandle) ', $path_or_fh, '.';

    return $contents;
}

process_commandline();

my $article_path = $ARGV[0];

if ( !ensure_sensible_article($article_path) ) {
    exit 1;
}

# Read the article
my $article_content =
       slurp( $article_path eq $DASH ? \*STDIN : $article_path )
    or croak( 'Error: unable to slurp ', $article_path, $FULL_STOP );

# Set up our HTTP user agent
my $agent = LWP::UserAgent->new( timeout => $timeout );

# Prepare the HTTP request
my $request_uri =
    "${MIZAR_PARSER_BASE_URI}?strictness=${transform}&format=${format}";
my $request = HTTP::Request->new( GET => $request_uri );
$request->content_type('application/x-www-form-urlencoded');
$request->content($article_content);

if ($debug) {
    print 'DEBUG: The request URI is: ', $request_uri, "\N{LF}";
}

# (Try to) send out the request
my $response = eval { $agent->request($request) };
my $eval_request_message = $EVAL_ERROR;

if ( defined $response ) {
    if ( $response->is_success() ) {

        my $response_content = $response->content();
        print $response_content, "\N{LF}";

        exit 0;

    }
    else {

        my $status = $response->status_line();
        if ( defined $status ) {

            my $response_content = $response->content();
            $response_content =
                defined $response_content ? $response_content : $EMPTY_STRING;

            print {*STDERR}
                'Error: the request was unsuccessful.  The parsing service returned:',
                "\N{LF}", "\N{LF}", $TWO_SPACES, $status,
                "\N{LF}", "\N{LF}";

            if ( $response_content eq $EMPTY_STRING ) {

                print {*STDERR}
                    'The message received from the parsing service is empty.',
                    "\N{LF}";

            }
            else {

                print {*STDERR} 'Here is the message we received:',
                    "\N{LF}", "\N{LF}";
                print {*STDERR} $response_content, "\N{LF}";

            }

        }
        else {

            print {*STDERR}
                'Error: the request was unsuccessful, and it seems we did not even get a response from the server.',
                "\N{LF}",
                '(Did the request timeout?  The value of the timeout was ',
                $timeout, ' seconds.)', "\N{LF}";

        }

        exit 1;
    }
}
else {
    if ( defined $eval_request_message ) {
        print {*STDERR}
            'Error: the HTTP request failed.  Here is the message we got:',
            "\N{LF}", $eval_request_message, "\N{LF}";
    }
    else {
        print {*STDERR}
            'Error: the HTTP request failed badly; not even a failure message is available.',
            "\N{LF}";
    }
}

__END__

=pod

=encoding utf8

=head1 NAME

mizparse - Parse and transform B<Mizar> texts

=head1 USAGE

mizparse.pl [options] mizar-article

=head1 EXAMPLES

=head2 Supply a file argument

C<mizarparse.pl article.miz>

=head2 Read from standard input

C<cat article.miz | mizparse.pl ->

=head2 Do the Weakly Strict Mizar transformation, get the results in XML

C<mizarparse.pl --transform=wsm article.miz>

=head2 Do the More Strict Mizar transformation, plain text output

C<mizarparse.pl --transform=wsm --format=text article.miz>

=head1 REQUIRED ARGUMENTS

It is mandatory to supply a B<Mizar> article.  That can be done in two
ways: by supplying the name of a B<Mizar> file, which will be read, or by
supplying the special name C<->, in which case standard input will be
read and interpreted as a B<Mizar> file.

=over 8

The latter interpretation takes priority: if there is a file whose
name is C<->, it will be ignored and standard input will be consulted.
(Are you sure you want a file called C<->?)

=back

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

During execution, the program will output indications of what it is
doing.

=item B<--debug>

During execution, the program will output even more indications of
what it is doing.  (Setting this value implies setting B<--verbose>.)

=item B<--format>

Specify the desired format of the output.  Two output formats are
available: text (indicated by using B<text> for this option) and XML
(indicated by using B<xml>).  By default, XML is output.

=item B<--transform>

Specify the desired transformation of the text.  Three transformations
are presently defined:

=over 8

=item none

Use B<none>.  No transformation of the text will be carried out; your
text will be used as-is.

=item Weakly Strict Mizar

Use B<wsm>.  The Weakly Strict Mizar transformation will be carried
out.

=item More Strict Mizar

Use B<msm>.  The More Strict Mizar transformation will be carried out.

=back

By default, no transformation will be carried out (i.e., the default
value is B<none>).

=back

=head1 DESCRIPTION

B<mizparse> takes a B<Mizar> file as its sole argument and submits it to
L<the Mizar parsing service|http://mizar.cs.ualberta.ca/parsing/>.
One controls two variables: the transformation and the format of the
response.

If the supplied article is called C<->, then standard input will be
read (even if there is, curiously, a file in the current working
directory called "-").  Using C<-> as the B<Mizar> file allows
B<mizparse> to be used as part of a text processing toolchain.

=head1 EXIT STATUS

As usual for commandline applications, the exit value of B<mizparse>
is 0 for success and non-zero for non-success.  (In case the HTTP
request succeeds, in the sense that a response is successfully
received but its HTTP return code is greater than or equal to 400, the
exit value of this program will be non-zero.)

=head1 BUGS AND LIMITATIONS

One cannot specify the version of the B<Mizar> library that one wishes to
use.  At the moment, B<Mizar> system version B<7.13.01> and MML version
B<4.181.1147> is assumed.

=head1 INCOMPATIBILITIES

No incompatibilities are known.  If you find any, please contact the
author (see below for contact information).

=head1 DEPENDENCIES

This program depends on the following Perl packages, all either
standard (come with Perl whether you like it or not) or available on
CPAN:

=over 8

=item L<LWP|http://search.cpan.org/~gaas/libwww-perl-6.04/lib/LWP.pm>

=item L<Getopt::Long|http://search.cpan.org/~jv/Getopt-Long-2.38/lib/Getopt/Long.pm>

=item L<Pod::Usage|http://search.cpan.org/~marekr/Pod-Parser-1.51/lib/Pod/Usage.pm>

=item L<Readonly|http://search.cpan.org/~roode/Readonly-1.03/Readonly.pm>

=item L<English|http://search.cpan.org/~flora/perl-5.14.2/lib/English.pm>

=item L<Carp|http://search.cpan.org/~zefram/Carp-1.25/lib/Carp.pm>

=back

To my knowledge I know of no essential version dependencies of these
packages, all of which are quite old and to which, therefore (...)
there is (probably) a fairly stable interface.  I could enforce, at
compile time, that these modules are loaded with the precise versions
that you can see in the CPAN URIs, but I have some hope that somewhat
older versions of these modules suffice for the rather straightforward
tasks that I require of them.

=head1 DIAGNOSTICS

If you are not getting the output you expect, try executing the same
command in verbose mode (add B<--verbose> to the invocation of this
program), or, better (worse?), debug mode (B<--debug>).

=head1 CONFIGURATION

No external coniguration needed to run the program; it should work Out
Of The Box.  (Assuming you can even run it at all; see the section on
Dependencies.)  A working network connection is required; the program
will submit an HTTP GET request on port 80 on a Canadian server in an
undisclosed location.

Even if you have a working B<Mizar> environment on your own machine, it
will not be consulted.  The only thing that matters is the content of
the B<Mizar> file that you give to this program.

=head1 SEE ALSO

=over 8

=item L<The Mizar parsing service homepage|http://mizar.cs.ualberta.ca/parsing/>.

The parsing service underlies this program.

=item L<The Mizar homepage|http://mizar.org>

The official web resource for B<Mizar>.

=item L<Mizar: The first 30 years|http://markun.cs.shinshu-u.ac.jp/mizar/mma.dir/2005/mma2005(2).pdf>, by Roman Matuszewski and Piotr Rudnicki, I<Mechanized Mathematics and Its Applications>, B<4>(1), 2005, 3E<ndash>24.

An overview of some of the history of B<Mizar>.

=item L<Mizar in a nutshell|http://jfr.cib.unibo.it/article/download/1980/1356>, by Adam Grabowski, Artur KorniE<322>owicz, and Adam Naumowicz, I<Journal of Formalized Reasoning> B<3>(2), 2010, pp. 153E<ndash>245.

A recent reference about the system.

=back

=head1 AUTHOR

Jesse Alama (j.alama@fct.unl.pt).  Any feedback is welcome.

=head1 LICENSE AND COPYRIGHT

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
