#!perl

=pod

=encoding UTF8

=head1 NAME

ChartDB.pm - NOAA Chart and Chart Corrections download utility

=cut

package ChartDB;

use v5.18;
use strict;
use warnings;

=head1 VERSION

Version 0.05

=cut

our $VERSION = 0.05;

use Carp;
use Path::Tiny;
use WWW::Mechanize::GZip;
use Log::Log4perl qw(:easy :levels);
use IO::Uncompress::Unzip qw($UnzipError);

=head1 SYNOPSIS

ChartDB.pm is a Perl module with submodules and a default run routine which can automatically download NOAA charts.

    perl ChartDB.pm

loads the CHARTS.TXT manifest and starts downloading chart files.

=head1 DESCRIPTION

The module will first scan a manifest file (CHARTS.TXT) to initialize its list of charts to process.  Once the manifest is loaded, the chart storage directory is scanned for any additional charts added after the manifest was created.  Each chart on the list is mirrored from the NOAA webserver.  Raster charts (RNC's) present on the list trigger the download of chart corrections for those raster charts. Additional chart corrections to be downloaded can be contained in the manifest or in the directory structure.

Once the downloads are complete, the downloaded files are unzipped to their respective locations in the charts database path structure.  ChartDB.pm uses the following structure to facilitate usage with OpenCPN:

  ./BSB_ROOT
  ./ENC_ROOT
  ./COR_ROOT

ChartDB.pm uses the following working directory for zipfiles: 

  ./ZIP_ROOT

Charts are named by a chart number from NOAA, and each chart is stored in a 
sub directory labeled by the chart name (number):

  ./BSB_ROOT/16709
  ./ENC_ROOT/US5VA27M
  ./COR_ROOT/16709

Once the unzipping process is completed, the new manifest is written and the chart corrections are all collated into a text file.

=cut

{

    package ChartDB::Manifest;
    use v5.18;
    use Carp;
    use Scalar::Util qw(blessed);
    use Moose;
    with 'MooseX::Log::Log4perl';

    has 'path' => (
        is  => 'ro',
        isa => 'Path::Tiny'
    );

    has 'seenenc' => (
        is      => 'ro',
        isa     => 'HashRef',
        traits  => ['Hash'],
        default => sub { { US3AK2HM => undef } }
    );

    has 'seenrnc' => (
        is      => 'ro',
        isa     => 'HashRef',
        traits  => ['Hash'],
        default => sub { { 16700 => undef } }
    );

    has 'seencor' => (
        is      => 'ro',
        isa     => 'HashRef',
        traits  => ['Hash'],
        default => sub { { 16700 => undef } }
    );

    has 'enc' => (
        is      => 'ro',
        isa     => 'ArrayRef[Str]',
        traits  => ['Array'],
        default => sub { [qw( US3AK2HM )] }
    );

    has 'rnc' => (
        is      => 'ro',
        isa     => 'ArrayRef[Str]',
        traits  => ['Array'],
        default => sub { [qw( 16700 )] }
    );

    has 'cor' => (
        is      => 'ro',
        isa     => 'ArrayRef[Str]',
        traits  => ['Array'],
        default => sub { [qw( 16700 )] }
    );

    has 'update' => (
        is      => 'ro',
        isa     => 'ArrayRef[Str]',
        traits  => ['Array'],
        default => sub { [qw( 16700 )] }
    );

    has 'updateseen' => (
        is      => 'ro',
        isa     => 'HashRef',
        traits  => ['Hash'],
        default => sub { { 16700 => undef } }
    );

    sub load {
        my $self = shift;
        croak "load error: must be called on blessed object!"
          unless blessed($self);

        my $path = $self->path();
        $self->log->logdie("load error: must have a path object!")
          unless blessed($path);

        $self->log->info("Loading manifest, if available... ");

        return unless $path->exists();

        my $st = $path->stat();

        return unless -r $st;

        my @ring = $path->lines();

        #Path::Tiny could use a read next (count) lines...
        my $line;
        for $line (@ring) {
            my ( $type, $name ) = split( ":", $line );
            if ( defined($type) and defined($name) ) {
                for ($type) {
                    $self->add_enc($name) if /enc/;
                    $self->add_rnc($name) if /rnc/;
                    $self->add_cor($name) if /cor/;
                    default {};
                }
            }
        }
        $self->log->info("Load Manifest: done.");
    }

    sub save {
        my $self = shift;
        croak "load error: must be called on blessed object!"
          unless blessed($self);

        my $path = $self->path();
        $self->log->logdie("load error: must have a path object!")
          unless blessed($path);

        $self->log->info( "Saving manifest as " . $path->basename() . "... " );

        my @lines = ();

        #Path::Tiny could use a read next (count) lines...
        my $type;
        my $name;
        for $type (qw(cor enc rnc)) {
            print uc($type) . " list: ";
            for $name ( @{ $self->$type() } ) {
                my $line = join( ":", $type, $name, "\n" );
                push @lines, $line;
            }
        }
        $path->spew(@lines);
        $self->log->info("Save Manifest: done.");
    }

    sub add_enc {
        my $self    = shift;
        my $encname = shift;
        my $seen    = $self->seenenc();
        return if exists( $seen->{$encname} );
        _add_item( $self->enc(), $seen, $encname );
        return 1;
    }

    sub add_rnc {
        my $self    = shift;
        my $rncname = shift;

        #every RNC should have a correction associated with it, regardless
        #of Manifest list of corrections.
        #maybe later this can be controlled with an option to disable autofill
        $self->add_cor($rncname);
        my $seen = $self->seenrnc();
        return if exists( $seen->{$rncname} );
        _add_item( $self->rnc(), $seen, $rncname );
        return 1;
    }

    sub add_cor {
        my $self    = shift;
        my $corname = shift;
        my $seen    = $self->seencor();
        return if exists( $seen->{$corname} );
        _add_item( $self->cor(), $seen, $corname );
        return 1;
    }

    sub add_update {
        my $self       = shift;
        my $updatename = shift;
        my $seen       = $self->updateseen();
        return if exists( $seen->{$updatename} );
        _add_item( $self->update(), $seen, $updatename );
        return 1;
    }

    sub _add_item {
        my ( $list, $seen, $item ) = @_;
        push @{$list}, $item;
        $seen->{$item} = undef;
    }

    1;
}

{

    package ChartDB::Base;
    use Moose;
    with 'MooseX::Log::Log4perl';

    has 'type' => ( is => 'ro', isa => 'Str', default => 'NONE' );
    has 'name' => ( is => 'ro', isa => 'Str' );
    has 'dldate' => ( is => 'ro', isa => 'DateTime' );
    has 'path'   => ( is => 'ro', isa => 'Path::Tiny' );
    has 'source' => ( is => 'ro', isa => 'URI::URL' );
    1;
}

{

    package ChartDB::ENC;

    use Moose;
    extends 'ChartDB::Base';
    with 'MooseX::Log::Log4perl';

    has 'type' => ( is => 'ro', default => 'ENC' );
    1;
}

{

    package ChartDB::RNC;

    use Moose;
    extends 'ChartDB::Base';
    with 'MooseX::Log::Log4perl';

    has 'type' => ( is => 'ro', default => 'RNC' );
    1;
}

{

    package ChartDB::COR;
    use Moose;
    extends 'ChartDB::Base';
    with 'MooseX::Log::Log4perl';

    has 'type' => ( is => 'ro', default => 'COR' );
    1;
}

my $ntm_re = '([a-zA-Z]{3})(?:\\s+)(\\d+)(?:\\/)(\\d+)(?:,\\s+)(.+)';
our $ntm_re_qr = qr/$ntm_re/i;

#$1 = CNM|LNM|WNM    $2 = WEEK    $3 = YEAR    $4 = Agency


use Moose;

=head1 ATTRIBUTES

=head2 C<log>

C<log> is a Log4perl logger object.

=cut

with 'MooseX::Log::Log4perl';

=head2 C<manifest>

C<manifest> is a ChartDB::Manifest object which stores the list of ENC, RNC and COR chart items to process.

=cut

has 'manifest' => (
    is      => 'ro',
    isa     => 'ChartDB::Manifest',
    lazy    => 1,
    builder => '_build_manifest'
);

=head2 C<webclient>

C<webclient> is a WWW:Mechanize::GZip object to handle http requests and mirroring.

=cut

has 'webclient' => (
    is       => 'ro',
    required => 1,
    isa      => 'WWW::Mechanize::GZip'
);

=head2 C<PARENT>

C<PARENT> is a Path::Tiny object containing the root of the chart database, the parent directory.

=cut

has 'PARENT' => (
    is       => 'ro',
    default  => sub { Path::Tiny->cwd() },
    required => 1,
    isa      => 'Path::Tiny',
    init_arg => 'DBROOT'
);

#ENC or BSB or COR
#electronic or raster or corrections
#unfortunately NOAA chose BSB instead of RNC for their raster chart directory

=head2 C<cor_root>

C<cor_root> is the chart database subdirectory for the chart corrections files.

=cut

has 'cor_root' => (
    is      => 'ro',
    isa     => "Path::Tiny",
    default => sub { Path::Tiny->cwd->child('COR_ROOT') }
);

=head2 C<enc_root>

C<enc_root> is the chart database subdirectory for the vector chart files.

=cut

has 'enc_root' => (
    is      => 'ro',
    isa     => "Path::Tiny",
    default => sub { Path::Tiny->cwd->child('ENC_ROOT') }
);

=head2 C<rnc_root>

C<enc_root> is the chart database subdirectory for the raster chart files.

=cut

has 'rnc_root' => (
    is      => 'ro',
    isa     => "Path::Tiny",
    default => sub { Path::Tiny->cwd->child('BSB_ROOT') }
);

=head2 C<zip_root>

C<zip_root> is the chart database subdirectory for the zipped chart files.

=cut

has 'zip_root' => (
    is      => 'ro',
    isa     => "Path::Tiny",
    default => sub { Path::Tiny->cwd->child('ZIP_ROOT') }
);

=head2 C<failures>

C<failures> stores the list of failed downloads for subsequent retries and reporting.

=cut

has 'failures' => (
    is      => 'ro',
    traits  => ['Array'],
    default => sub { [] },
    isa     => 'ArrayRef[Str]'
);

=head2 C<bsb_form>,C<kap_form>, C<zip_form>, C<txt_form>, C<htm_form>

C<bsb_form> et al are filename formats for various filetypes.

=cut

has 'bsb_form' => ( is => 'ro', isa => 'Str', default => '%s.BSB' );
has 'kap_form' => ( is => 'ro', isa => 'Str', default => '%s.KAP' );
has 'zip_form' => ( is => 'ro', isa => 'Str', default => '%s.ZIP' );
has 'txt_form' => ( is => 'ro', isa => 'Str', default => '%s.TXT' );
has 'htm_form' => ( is => 'ro', isa => 'Str', default => '%s.HTML' );

=head2 C<cor_url_form>

C<cor_url_form> represents the url format for retrieving chart corrections from OCSDATA.

=cut

has 'cor_url_form' => (
    is      => 'ro',
    isa     => 'Str',
    default => 'http://ocsdata.ncd.noaa.gov/ntm/Listing_Text.aspx?Chart=%s'
);

=head2 C<enc_url_form>

C<enc_url_form> represents the url format for retrieving vector charts from NOAA.

=cut

has 'enc_url_form' => (
    is      => 'ro',
    isa     => 'Str',
    default => 'http://www.charts.noaa.gov/ENCs/%s.zip'
);

=head2 C<rnc_url_form>

C<rnc_url_form> represents the url format for retrieving raster charts from NOAA.

=cut

has 'rnc_url_form' => (
    is      => 'ro',
    isa     => 'Str',
    default => 'http://www.charts.noaa.gov/RNCs/%s.zip'
);

=head1 METHODS

=head2 C<BUILDARGS>

C<BUILDARGS> is part of the Moose object system. This around BUILDARGS wrapper handles arguments as a hashref, arrayref or list.  It takes care of promoting a DBROOT string parameter to a Path::Tiny object for later use.

=cut

around BUILDARGS => sub {
    my $orig  = shift;
    my $class = shift;
    if ( @_ == 1 && !ref $_[0] ) {

        #single argument - and not a reference
        my $parent = Path::Tiny->new( $_[0] );
        croak "BUILDARGS error: could not make a Path::Tiny object"
          unless blessed($parent);
        return $class->$orig( DBROOT => $parent );
    }
    else {
        my %args;
        if ( @_ == 1 && ( ref( $_[0] ) eq 'HASH' ) ) {
            %args = %{ $_[0] };
        }
        if ( @_ == 1 && ( ref( $_[0] ) eq 'ARRAY' ) ) {
            %args = ( @{ $_[0] } );
        }
        if ( ( @_ % 2 ) == 0 ) { %args = (@_); }

        if ( defined( $args{'DBROOT'} ) ) {
            my $parent = Path::Tiny->new( $args{'DBROOT'} );
            croak "BUILDARGS error: could not make a Path::Tiny object"
              unless blessed($parent);
            $args{'DBROOT'} = $parent;

            # I only have to promote the init_arg,
            #Mouse/Moose takes care of MOVING the init_arg
            #reference to the final attribute. The below
            #two lines are unnecessary. Deleting the init_arg
            #here will trigger the default assignment, wiping
            #out the new arguments.
            #	    $args{'PARENT'} = $parent;
            #	    delete($args{'DBROOT'});
        }
        return $class->$orig(%args);
    }
};

=head2 C<add_failure>

C<add_failure> adds a download failure to the list for subsequent reporting.

=cut

sub add_failure {
    my $self = shift;
    my $name = shift;

    my $list = $self->failures();
    push @{$list}, $name;
    $self->log->info( 'Failed to download: ' . $name );
}

sub _build_manifest {

    #builds the manifest attribute in the ChartDB object.
    # this is a lazy attribute, to allow the root of the
    # ChartDB object to be fully setup first.
    my $self  = shift;
    my $path  = $self->PARENT();
    my $manif = $path->child('CHARTS.TXT');

    return ChartDB::Manifest->new( path => $manif );
}

=head2 C<load_directory>

C<load_directory> places the directory scanning code in one place. Uses coderef callbacks to add the items to the proper list according to the caller.

=cut

sub load_directory {
    my $self = shift;
    croak "load_directory error: must be called on a blessed object!"
      unless blessed($self);

    my $directory = shift;
    $self->log->logdie("load_directory error:  must be supplied a directory!")
      unless defined($directory);

    my $list = shift;
    $self->log->logdie(
        "load_directory error:  must be supplied an arrayref list!")
      unless ref($list) eq "ARRAY";

    my $code_ref = shift;
    $self->log->logdie(
        "load_directory error:  must be supplied a coderef callback")
      unless ref($code_ref) eq 'CODE';

    my $root = $self->PARENT();
    $self->log->logdie(
        "load_directory error: PARENT() is not a Path::Tiny object")
      unless blessed($root);

    my $p = $root->child($directory);
    $self->log->logdie(
        "load_directory error: child() is not a Path::Tiny object")
      unless blessed($p);
    $self->log->info( 'Scanning: ' . $directory );
    my $f;
    my $name;
    my $manifest = $self->manifest();
    for $f ( $p->children() ) {

        if ( $f->is_dir() ) {
            $name = $f->basename;
            $manifest->$code_ref($name);
        }
    }
    $self->log->info('Scanning: done.');
    my $n = @$list;
    return $n;
}

=head2 C<load_cor_root>

C<load_cor_root> loads the toplevel directory for chart corrections.

=cut

sub load_cor_root {
    my $self = shift;
    croak "load_cor_root error: must be called on a blessed object!"
      unless blessed($self);

    my $manifest = $self->manifest();

    my $code_ref = $manifest->can('add_cor');

    my $n =
      $self->load_directory( $self->cor_root(), $manifest->cor(), $code_ref );
    $self->log->info( "Found $n charts in " . $self->cor_root() );
}

=head2 C<load_enc_root>

C<load_enc_root> loads the toplevel directory for vector charts.

=cut

sub load_enc_root {
    my $self = shift;
    croak "load_enc_root error: must be called on a blessed object!"
      unless blessed($self);

    my $manifest = $self->manifest();

    my $code_ref = $manifest->can('add_enc');

    my $n =
      $self->load_directory( $self->enc_root(), $manifest->enc(), $code_ref );
    $self->log->info( "Found $n charts in " . $self->enc_root() );
}

=head2 C<load_rnc_root>

C<load_rnc_root> loads the toplevel directory for raster charts.

=cut

sub load_rnc_root {
    my $self = shift;
    croak "load_rnc_root error: must be called on a blessed object!"
      unless blessed($self);

    my $manifest = $self->manifest();

    my $code_ref = $manifest->can('add_rnc');

    my $n =
      $self->load_directory( $self->rnc_root(), $manifest->rnc(), $code_ref );
    $self->log->info( "Found $n charts in " . $self->rnc_root() );
}

=head2 C<load_roots>

C<load_roots> loads the toplevel directory for each item type to be downloaded. Adds existing chart directories to the manifest.

=cut

sub load_roots {
    my $self = shift;
    croak "load_roots error: must be called on a blessed object!"
      unless blessed($self);

    $self->log->info("Loading existing chart lists... ");

    $self->load_cor_root();
    $self->load_enc_root();
    $self->load_rnc_root();

    $self->log->info('Load existing chart lists: done. ');
}

=head2 C<init_roots>

C<init_roots> initializes the toplevel directories for each type of chart item to be downloaded. Ensures at least the minimum chart directories listed in the original manifest are present.

=cut

sub init_roots {
    my $self = shift;
    croak "init_roots error: must be called on a blessed object!"
      unless blessed($self);

    $self->log->info("Initializing root folders... ");

    my $manifest = $self->manifest();
    $self->init_cor_root( $manifest->cor() );
    $self->init_enc_root( $manifest->enc() );
    $self->init_rnc_root( $manifest->rnc() );
    $self->log->info("Initialized root folders: done.");
}

=head2 C<init_root>

C<init_root> initializes a toplevel directory for any type of chart item to be downloaded. Ensures at least the minimum chart directories listed in the original manifest are present.

=cut

sub init_root {
    my $self = shift;
    croak "init_root error: requires a blessed object!"
      unless blessed($self);

    my $charts = shift;
    $self->log->logdie("init_root error: requires an array reference!")
      unless ref($charts) eq "ARRAY";

    my $directory = shift;
    $self->log->logdie("init_root error: requires a defined directory")
      unless defined($directory);

    my $parent = $self->PARENT();
    my $root   = $parent->child($directory);

    my $found = 0;
    my $added = 0;

    my $chart;
    foreach $chart ( @{$charts} ) {
        my $path = $root->child($chart);
        if ( $path->is_dir() ) {
            $found++;
            next;
        }
        $path->mkpath();
        $added++;
    }
    $self->log->info( "Found $found items from manifest in " . $directory );
    $self->log->info( "Added $added items from manifest to " . $directory );
}

=head2 C<init_cor_root>

C<init_cor_root> initializes the chart corrections toplevel directory.

=cut

sub init_cor_root {
    my $self = shift;
    croak "init_cor_root error: requires a blessed object!"
      unless blessed($self);

    my $charts = shift;
    $self->log->logdie("init_cor_root error: requires an array reference!")
      unless ref($charts) eq "ARRAY";

    $self->init_root( $charts, $self->cor_root() );
}

=head2 C<init_enc_root>

C<init_enc_root> initializes the vector chart toplevel directory.

=cut

sub init_enc_root {
    my $self = shift;
    croak "init_enc_root error: requires a blessed object!"
      unless blessed($self);

    my $charts = shift;
    $self->log->logdie("init_enc_root error: requires an array reference!")
      unless ref($charts) eq "ARRAY";

    $self->init_root( $charts, $self->enc_root() );
}

=head2 C<init_rnc_root>

C<init_rnc_root> initializes the raster chart toplevel directory.

=cut

sub init_rnc_root {
    my $self = shift;
    croak "init_rnc_root error: requires a blessed object!"
      unless blessed($self);

    my $charts = shift;
    $self->log->logdie("init_rnc_root error: requires an array reference!")
      unless ref($charts) eq "ARRAY";

    $self->init_root( $charts, $self->rnc_root() );
}

=head2 C<get_zip_path>

C<get_zip_path> gets the path name expected for zipfile of chart corrections for a given chart.

=cut

sub get_zip_path {
    my $self = shift;
    croak "get_zip_path error: requires a blessed object!"
      unless blessed($self);

    my $chart = shift;
    $self->log->logdie("get_zip_path error: requires a defined chart name!")
      unless defined($chart);

    my $nc_root = $self->zip_root();
    $self->log->logdie("get_zip_path error: requires a defined root!")
      unless defined($nc_root);

    my $file_form = $self->zip_form();
    $self->log->logdie("get_zip_path error: requires a defined filename form!")
      unless defined($file_form);

    return $self->get_nc_path( $chart, $nc_root, $file_form );
}

=head2 C<get_nc_path>

C<get_nc_path> gets a nautical chart path name for any type of chart database item.

=cut

sub get_nc_path {
    my $self = shift;
    croak "get_nc_path error: requires a blessed object!"
      unless blessed($self);

    my $chart = shift;
    $self->log->logdie("get_nc_path error: requires a defined chart name!")
      unless defined($chart);

    my $nc_root = shift;
    $self->log->logdie("get_nc_path error: requires a defined root!")
      unless defined($nc_root);

    my $file_form = shift;
    $self->log->logdie("get_nc_path error: requires a defined filename form!")
      unless defined($file_form);

    # /username/home/Documents/Charts
    my $parent = $self->PARENT();

    #/username/home/Documents/Charts/BSB_ROOT
    #/username/home/Documents/Charts/COR_ROOT
    #/username/home/Documents/Charts/ENC_ROOT
    my $root = $parent->child($nc_root);

    #/username/home/Documents/Charts/BSB_ROOT/16700
    #/username/home/Documents/Charts/COR_ROOT/16700
    #/username/home/Documents/Charts/ENC_ROOT/US5..K11M
    my $path = $root->child($chart);

    #/username/home/Documents/Charts/BSB_ROOT/16700/16700.BSB
    #/username/home/Documents/Charts/COR_ROOT/16700/16700.TXT
    #/username/home/Documents/Charts/ENC_ROOT/US5..K11M/US5..K11M.000
    my $filename = sprintf( $file_form, $chart );
    return $path->child($filename);
}

=head2 C<cor_path>

C<cor_path> gets the expected path name for a corrections file for a chart once unzipped to text format

=cut

sub cor_path {
    my $self = shift;
    croak "cor_path error: requires a blessed object!"
      unless blessed($self);

    my $chart = shift;
    $self->log->logdie("cor_path error: requires a defined chart name!")
      unless defined($chart);

    my $path =
      $self->get_nc_path( $chart, $self->cor_root(), $self->txt_form() );
    return $path;
}

=head2 C<enc_path>

C<enc_path> gets the expected path name for a vector chart file for a chart once unzipped.

=cut

sub enc_path {
    my $self = shift;
    croak "enc_path error: requires a blessed object!"
      unless blessed($self);

    my $chart = shift;
    $self->log->logdie("enc_path error: requires a defined chart name!")
      unless defined($chart);

    my $path =
      $self->get_nc_path( $chart, $self->enc_root(), $self->enc_form() );
    return $path;
}

=head2 C<rnc_path>

C<rnc_path> gets the expected path name for a raster chart file for a chart once unzipped.

=cut

sub rnc_path {
    my $self = shift;
    croak "rnc_path error: requires a blessed object!"
      unless blessed($self);

    my $chart = shift;
    $self->log->logdie("rnc_path error: requires a defined chart name!")
      unless defined($chart);

    my $path =
      $self->get_nc_path( $chart, $self->rnc_root(), $self->rnc_form() );
    return $path;
}

=head2 C<mirror_lists>

C<mirror_lists> mirrors the files listed in the manifest for each type of chart object.

=cut

sub mirror_lists {
    my $self = shift;

    $self->log->info("Mirroring chart files and updates... ");

    my $n = 0;
    $self->log->info("Correction files...");
    $n += $self->mirror_cor_list();
    $self->log->info("Correction files: done.");

    $self->log->info("ENC files...");
    $n += $self->mirror_enc_list();
    $self->log->info("ENC files: done.");

    $self->log->info("RNC files...");
    $n += $self->mirror_rnc_list();
    $self->log->info("RNC Files: done.");

    return $n;
}

sub _mirror {
    my ( $self, $mech, $u, $p ) = @_;

    my $exception;
    my $e = \$exception;

    $p->parent()->mkpath() unless $p->parent()->is_dir();

    my $rc;
    {
        local $@;
        ${$e} = undef;

        eval {
            local $SIG{'__DIE__'};
            $rc = $mech->mirror( $u, $p );
        };
        ${$e} = $@;
    }
    if ( defined( ${$e} ) and ${$e} ne '' ) {
        return $e;
    }

    return $rc;
}

=head2 C<mirror_cor_list>

C<mirror_cor_list> mirrors the list of corrections from the NOAA OCSDATA site.

Unfortunately, with the gzipped text results files at ocsdata.ncd.noaa.gov being dynamically generated, every time this is run it will retrieve the whole set of corrections.  Mirroring is not possible, because the server file is always new "right now." Someday they might provide metadata separately to ease the server load.
    
=cut

sub mirror_cor_list {
    my $self = shift;
    croak "mirror_cor_list error: requires a blessed object!"
      unless blessed($self);

    my $manifest = $self->manifest();
    $self->log->logdie("mirror_cor_list error: requires a manifest object!")
      unless blessed($manifest);

    my $charts = $manifest->cor();
    $self->log->logdie("mirror_cor_list error: requires a defined list!")
      unless ref($charts) eq 'ARRAY';

    my $mech = $self->webclient();
    $self->log->logdie(
        "mirror_cor_list error: requires a WWW::Mechanize::GZip webclient!")
      unless blessed($mech);

    my $url_form = $self->cor_url_form();
    $self->log->logdie("mirror_cor_list error: requires a defined url form")
      unless defined($url_form);

    my $n = 0;
    my $rc;
    my $chart;
    for $chart ( @{$charts} ) {
        next unless defined($chart);
        my $u = sprintf( $url_form, $chart );
        my $p = $self->cor_path($chart);
        printf( "%8s", $chart );
        $rc = $self->_mirror( $mech, $u, $p );

        if ( ref($rc) eq 'SCALAR' ) {

            #caught exception (probably read timeout) from
            #WWW::Mechanize::GZip.
            print '* ';
            my $exception = ${$rc};
            $self->add_failure( $chart, $exception );
            next;
        }
        $self->log->logdie('$rc must be an HTTP::Response object!')
          unless blessed($rc);

        if ( $rc->is_success() or $rc->code() == 304 ) {
            print ", ";
            $n++;
        }
        else {
            print '! ';
            $self->add_failure( $chart, $rc->code() );
        }
    }
    print "\n";
    return $n;
}

=head2 C<mirror_enc_list>

C<mirror_enc_list> mirrors the files listed on the ENC (vector chart) list in the manifest.

Some extra effort is required to catch the timeout exceptions that Mechanize often throws.

=cut

sub mirror_enc_list {
    my $self = shift;
    croak "mirror_enc_list error: requires a blessed object!"
      unless blessed($self);

    my $manifest = $self->manifest();
    $self->log->logdie("mirror_enc_list error: requires a manifest object!")
      unless blessed($manifest);

    my $charts = $manifest->enc();
    $self->log->logdie("mirror_enc_list error: requires a defined list!")
      unless ref($charts) eq 'ARRAY';

    my $mech = $self->webclient();
    $self->log->logdie(
        "mirror_enc_list error: requires a WWW::Mechanize::GZip webclient!")
      unless blessed($mech);

    my $url_form = $self->enc_url_form();
    $self->log->logdie("mirror_enc_list error: requires a defined url form")
      unless defined($url_form);

    my $n = 0;
    my $rc;
    my $chart;
    for $chart ( @{$charts} ) {
        next unless defined($chart);
        my $u = sprintf( $url_form, $chart );
        my $p = $self->get_zip_path($chart);

        printf( "%8s", $chart );
        $rc = $self->_mirror( $mech, $u, $p );

        if ( ref($rc) eq 'SCALAR' ) {

            #caught exception (probably read timeout) from
            #WWW::Mechanize::GZip.
            print '* ';
            my $exception = ${$rc};
            $self->add_failure( $chart, $exception );
            next;
        }
        $self->log->logdie('$rc must be an HTTP::Response object!')
          unless blessed($rc);

        if ( $rc->is_success() or $rc->code() == 304 ) {
            print ", ";
            $n++;
        }
        else {
            print '! ';
            $self->add_failure( $chart, $rc->code() );
        }
    }
    print "\n";
    return $n;
}

=head2 C<mirror_rnc_list>

C<mirror_rnc_list> mirrors the files listed on the RNC (raster chart) list in the manifest.

Some extra effort is required to catch the timeout exceptions that Mechanize often throws.

=cut

sub mirror_rnc_list {
    my $self = shift;
    croak "mirror_rnc_list error: requires a blessed object!"
      unless blessed($self);

    my $manifest = $self->manifest();
    $self->log->logdie("mirror_rnc_list error: requires a manifest object!")
      unless blessed($manifest);

    my $charts = $manifest->rnc();
    $self->log->logdie("mirror_rnc_list error: requires a defined list!")
      unless ref($charts) eq 'ARRAY';

    my $mech = $self->webclient();
    $self->log->logdie(
        "mirror_rnc_list error: requires a WWW::Mechanize::GZip webclient!")
      unless blessed($mech);

    my $url_form = $self->rnc_url_form();
    $self->log->logdie("mirror_rnc_list error: requires a defined url form")
      unless defined($url_form);

    my $n = 0;
    my $rc;
    my $chart;
    for $chart ( @{$charts} ) {
        next unless defined($chart);
        my $u = sprintf( $url_form, $chart );
        my $p = $self->get_zip_path($chart);

        printf( "%8s", $chart );
        $rc = $self->_mirror( $mech, $u, $p );

        if ( ref($rc) eq 'SCALAR' ) {

            #caught exception (probably read timeout) from
            #WWW::Mechanize::GZip.
            print '* ';
            my $exception = ${$rc};
            $self->add_failure( $chart, $exception );
            next;
        }
        $self->log->logdie('$rc must be an HTTP::Response object!')
          unless blessed($rc);

        if ( $rc->is_success() or $rc->code() == 304 ) {
            print ", ";
            $n++;
        }
        else {
            $self->add_failure( $chart, $rc->code() );
            print '! ';
        }
    }
    print "\n";
    return $n;
}

=head2 C<unzip_lists>

C<unzip_lists> unzips the files in each list which were retrieved as zipfiles.

=cut

sub unzip_lists {
    my $self = shift;

    $self->log->info("Unzipping chart files and updates... ");

    $self->log->info("Unzip ENC files...");
    $self->unzip_enc_list();
    $self->log->info("Unzip ENC files: done.");

    $self->log->info("Unzip RNC files...");
    $self->unzip_rnc_list();
    $self->log->info("Unzip RNC files: done.");

    $self->log->info("Unzip Corrections files...");
    $self->unzip_cor_list();
    $self->log->info("Unzip Corrections files: done.");

    $self->log->info("Unzipping chart files and updates: done.");

    #using GZipped Mechanize means they are just text files when saved.
}

=head2 C<unzip_cor_list>

C<unzip_cor_list> unzips the files in the list of corrections downloaded.

Currently does nothing, since the webclient unzips the downloads on retrieval.

=cut

sub unzip_cor_list {
    my $self = shift;

    $self->log->info(
        "Corrections files are already unzipped by the webclient: done.");

    #    my $self = shift;

    #    my $manifest = $self->manifest();

    #    my $list = $manifest->enc();

    #    my $chart;
    #    for $chart (@{$list}) {
    #	_unzip(
    #	    $self->get_zip_path($chart),
    #	    $self->PARENT()->child($self->enc_root())
    #	    );
    #    }
}

=head2 C<unzip_enc_list>

C<unzip_enc_list> unzips the ENC chart files from the downloaded ENC zipfiles.

Since the ENC zipfiles contain multiple chart files, we have to unzip them ourselves.

=cut

sub unzip_enc_list {
    my $self = shift;

    my $manifest = $self->manifest();

    my $list = $manifest->enc();

    my $chart;
    for $chart ( @{$list} ) {
        my $zpath = $self->get_zip_path($chart);

        unless ( $zpath->exists() and !$zpath->is_dir() ) {
            $self->add_failure($chart);
            next;
        }

        printf( "%8s", $chart );

        _unzip( $zpath, $self->PARENT()->child( $self->enc_root() ) );
        print ", ";
    }
    print "\n";
}

=head2 C<unzip_rnc_list>

C<unzip_rnc_list> unzips the RNC chart files from the downloaded RNC zipfiles.

Since the RNC zipfiles contain multiple chart files, we have to unzip them ourselves.

=cut

sub unzip_rnc_list {
    my $self = shift;

    my $manifest = $self->manifest();

    my $list = $manifest->rnc();

    my $chart;
    for $chart ( @{$list} ) {
        my $zpath = $self->get_zip_path($chart);

        unless ( $zpath->exists() and !$zpath->is_dir() ) {
            $self->add_failure($chart);
            next;
        }

        printf( "%8s", $chart );

        _unzip( $zpath, $self->PARENT()->child( $self->rnc_root() ) );
        print ", ";
    }
    print "\n";
}

sub _unzip {
    my $zipfile = shift;
    croak "_unzip error: must have Path::Tiny source file!"
      unless blessed($zipfile);

    my $targetpath = shift;
    croak "_unzip error: must have Path::Tiny target path!"
      unless blessed($targetpath);

    my $u = IO::Uncompress::Unzip->new( $zipfile->stringify() )
      or croak "Cannot open $zipfile: $UnzipError";

    my $status;
    for ( $status = 1 ; $status > 0 ; $status = $u->nextStream() ) {
        my $header = $u->getHeaderInfo();
        my $path   = $targetpath->sibling( $header->{'Name'} );
        unless ( $path->parent()->is_dir() ) {
            $path->parent()->mkpath
              or croak "couldn't find or make the target path: "
              . $path->parent();
        }
        if ( $path->basename =~ m!/$! ) {
            last if $status < 0;
            next;
        }    # if statement
        my $fh = $path->openw_raw;
        my $buff;
        while ( ( $status = $u->read($buff) ) > 0 ) {
            $fh->write($buff);
        }    # while loop
        $fh->close();
        my $stored_time = $header->{'Time'};
        utime( $stored_time, $stored_time, $path )
          or croak "Couldn't touch $path!";
    }    #for loop

    croak "Error processing $zipfile: $!" if $status < 0;
    return;
}    #unzip subroutine

=head2 C<collate_updates>

C<collate_updates> assembles a file with all updates to all charts in the database.  The intent was to arrive at a "Charts Corrected By" list such as is provided by NGA Notice to Mariners, but which is NOT provided by USCG Local Notice to Mariners.

The output file can be easily imported into another database for managing chart corrections on paper charts.

=cut

sub collate_updates {
    my $self = shift;
    croak "collate_updates error: requires a blessed object!"
      unless blessed($self);

#Chart: 530, Current Edition: 33, Print Date: Oct. /2010, North America West Coast San Diego to Aleutian Islands and Hawai‘ian Islands
#Chart	Action	Item Name	Charting Label	Latitude	Longitude	LatDD	LongDD	Published Document	Kapp	RNC Panel	RNC Posted

    $self->log->info("Collating updates...");

    my $manifest = $self->manifest();

    my $list = $manifest->cor();

    my $fn     = "ALL-NM.TXT";
    my $parent = $self->PARENT();
    my $all    = $parent->child($fn);

    $all->remove() if $all->exists();
    $all->touch();

    $fn = "LATEST-NM.TXT";
    my $latest = $parent->child($fn);

    $latest->remove() if $latest->exists();
    $latest->touch();

    my $chartname;

    my $slist =
      [ sort { sprintf( "%5s", $a ) cmp sprintf( "%5s", $b ) } ( @{$list} ) ];

    my $n = 0;
    for $chartname ( @{$slist} ) {
        my $chart = $self->cor_path($chartname);
        $n += _update_scanner( $chart, $all, $latest ) if $chart->exists();
    }
    $self->log->info("Collating updates: done.");
    $self->log->info("$n entries processed and saved to $fn");
}

sub _update_scanner {
    my $chartpath  = shift;
    my $allpath    = shift;
    my $latestpath = shift;

    unless ( $chartpath->exists() ) {
        croak "_update_scanner error: $chartpath not found!";
    }

    if ( $chartpath->is_dir() ) {
        croak "_update_scanner error: $chartpath is a directory!";
    }

    my $list;
    {

#from the Path::Tiny POD:
#	"lines_raw is like lines with a binmode of :raw. We use :raw instead of :unix so PerlIO buffering can manage reading by line."
# which means that \r\n to \n is handled automatically by lines_raw, and not by lines
        $list = [ $chartpath->lines_raw( { chomp => 1 } ) ];
    }
    my $n           = 0;
    my $corrections = {};
    _parse_chart_header( $corrections, $list );
    _parse_field_header( $corrections, $list );
    $n = _parse_corrections( $corrections, $list, $allpath, $latestpath );

    #   _find_latest_nm($corrections);

    #   _output_corrections($corrections, $targetpath);
    return $n;
}

sub _parse_chart_header {

#Chart: 530, Current Edition: 33, Print Date: Oct. /2010, North America West Coast San Diego to Aleutian Islands and Hawai‘ian Islands

    my $updates = shift;
    my $list    = shift;
    my $header  = shift @{$list};
    my @fields  = split( ', ', $header, 4 );
    $updates->{'Title'} = pop @fields;
    croak "_parse_chart_header error: Chart Title not found in:" . $header
      unless defined $updates->{'Title'};
    my $field;

    for $field (@fields) {
        my ( $key, $value ) = split( ': ', $field );
        if ( defined($key) and defined($value) ) {
            $updates->{$key} = $value;
        }
        else {
            croak "_parse_chart_header error: Unexpected undefined key. "
              . "Field is: "
              . $field
              if !defined($key);
            croak "_parse_chart_header error: Unexpected undefined value."
              . "Field is: "
              . $field
              if !defined($value);
        }
    }
}

sub _parse_field_header {

#Chart	Action	Item Name	Charting Label	Latitude	Longitude	LatDD	LongDD	Published Document	Kapp	RNC Panel	RNC Posted

    my $updates = shift;

    my $list = shift;

    my $header = shift( @{$list} );
    my $fields = [ split( "\t", $header ) ];

    $updates->{'FIELD_HEADERS'} = $fields;

    #setup output headers
    $updates->{'OUTPUT_HEADERS'} = [ @{$fields} ];
    my $outs  = $updates->{'OUTPUT_HEADERS'};
    my $first = shift @{$outs};
    unshift @{$outs}, 'Print Date';
    unshift @{$outs}, 'Current Edition';
    unshift @{$outs}, $first;
    push @{$outs}, 'Title';
}

sub _parse_corrections {
    my $updates    = shift;
    my $list       = shift;
    my $allpath    = shift;
    my $latestpath = shift;

    my @initlist =
      ( '_WNM', '_LNM', '_CNM', 'Current Edition', 'Print Date', 'Title' );

    $updates->{'CORRECTIONS'} //= [];
    $updates->{'_WNM'}        //= make_max_finder();
    $updates->{'_LNM'}        //= make_max_finder();
    $updates->{'_CNM'}        //= make_max_finder();

    my $n = 0;
    my $line;
    for $line ( @{$list} ) {
        my @items = split( "\t", $line );
        my $corr = {};
        my $xkey;
        for $xkey (@initlist) {
            $corr->{$xkey} = $updates->{$xkey};
        }

        my $idx;
        for $idx ( 0 .. $#items ) {
            my $key   = $updates->{'FIELD_HEADERS'}->[$idx];
            my $value = $items[$idx];
            $corr->{$key} = $value;
            if ( $key =~ 'Published Document' ) {
                _find_latest_nm( $corr, $key, $value );
            }
        }
        push @{ $updates->{'CORRECTIONS'} }, $corr;
        $allpath->append(
            _output_correction( $corr, $updates->{'OUTPUT_HEADERS'} ) );
        $n++;
    }
    $latestpath->append( _output_latest_nm( $updates->{'CORRECTIONS'}->[-1] ) )
      if ( @{ $updates->{'CORRECTIONS'} } > 0 );

    #Chart\tEdition\tPrint Date\tWNM\tLNM\tCNM
    return $n;
}

=head2 C<make_max_finder>

C<make_max_finder> returns a coderef which will return the maximum value from a list of values across multiple calls.  A clever closure.

=cut

sub make_max_finder {
    my $max;
    sub {
        for (@_) { $max = $_ if !defined $max || $_ > $max }
        $max;
      }
}

sub _find_latest_nm {
    my $self  = shift;
    my $key   = shift;
    my $value = shift;

    #530 is a good example with all three types in it.
    if ( $value =~ $ChartDB::ntm_re_qr ) {

        #$1 = CNM|LNM|WNM    $2 = WEEK    $3 = YEAR    $4 = Agency
        my ( $type, $week, $year, $agency ) = ( $1, $2, _expand_year($3), $4 );
        my $numval = 0 + $year * 100 + $week;
        for ($type) {
            $self->{'_CNM'}->($numval) if /CNM/i;
            $self->{'_LNM'}->($numval) if /LNM/i;
            $self->{'_WNM'}->($numval) if /WNM/i;
            default {};
        }
    }
}

sub _expand_year {
    my $year = shift;

    if ( $year < 50 ) {
        $year = 2000 + $year;
    }
    elsif ( $year > 50 and $year < 100 ) {
        $year = 1900 + $year;
    }
    return $year;
}

sub _output_correction {
    my $correction = shift;
    my $fields     = shift;

    my $field;
    my @items;
    for $field ( @{$fields} ) {
        push @items, $correction->{$field};
    }
    my $line = join( "\t", @items );
    return $line . "\n";
}

sub _output_latest_nm {
    my $correction = shift;

    my $fields //= [ 'Chart', 'Edition', 'Print Date', 'WNM', 'LNM', 'CNM' ];

    my $field;
    my @items;
    for $field ( @{$fields} ) {
        if ( $field =~ /WNM|LNM/i ) {
            my $maxnm = $correction->{ '_' . $field }->();
            push @items,
              sprintf( "%3s %04d w%02d",
                $field,
                int( $maxnm / 100 ),
                $maxnm - 100 * int( $maxnm / 100 ) )
              if defined($maxnm);
            push @items, '            ' unless defined($maxnm);
        }
        elsif ( $field =~ /CNM/i ) {
            my $maxnm = $correction->{ '_' . $field }->();
            push @items,
              sprintf( "%3s %04d m%02d",
                $field,
                int( $maxnm / 100 ),
                $maxnm - 100 * int( $maxnm / 100 ) )
              if defined($maxnm);
            push @items, '            ' unless defined($maxnm);
        }
        else {
            my $item = $correction->{$field};
            push @items, $item if defined($item);
            push @items, '            ' unless defined($item);
        }
    }
    my $line = join( "\t", @items );
    return $line . "\n";
}

=head2 C<run>

C<run> executes the main program.

Sets up logging, scans the manifest, augments the manifest with found directories, retrieves the charts, unzips the files, and collates the chart corrections.

=cut

sub run {

    Log::Log4perl->easy_init(
        {
            level => $INFO,
            file  => ">>ChartDB.log",

            #			category => "ChartDB"
        },
        {
            level => $INFO,
            file  => "STDOUT",

            #			category => "ChartDB"
        }
    );

    my $log = Log::Log4perl->get_logger("ChartDB");

    $log->trace( $0 . '->run() starting up chart downloader' );
    local $|;
    $|++;

    $log->info('Setting up webclient... ');
    use WWW::Mechanize::GZip;
    my $mech = WWW::Mechanize::GZip->new( autocheck => 0, stack_depth => 1 );
    $log->info('Setup webclient: done.');

    $log->info('Setting up Chart Database...');
    my $chartdb = ChartDB->new(
        'DBROOT'    => '.',
        'webclient' => $mech
    );
    $log->info('Setup Chart Database: done.');

    my $manifest = $chartdb->manifest();
    $manifest->load();

    $chartdb->init_roots();

    $chartdb->load_roots();

    $chartdb->mirror_lists();

    $chartdb->unzip_lists();

    $chartdb->collate_updates();

    $manifest->save();
}

run();

1;

=head1 AUTHOR

Michael P. Fabio, C<< <michaelpfabio at gmail.com> >>

=head1 BUGS

Use the issue tracker at github.com for this project. 

=head1 SUPPORT

Use the issue tracker at github.com for this project.

=head1 ACKNOWLEDGEMENTS

The free open source software movement is well deserving of praise. The efforts of the authors of, contributors to, and maintainers of the following are greatly appreciated:

=over 4

=item * Perl language L<https://www.perl.org/>

=item * Perl documentation L<http://perldoc.perl.org/>

=item * Perl modules on CPAN L<http://www.cpan.org/>

=item * NOAA for free Nautical Charts L<http://www.nauticalcharts.noaa.gov/>

=item * OpenCPN L<http://opencpn.org/ocpn/>

=back

Several routines are not-quite-verbatim answers to questions posed by others on various programming web forums.  The _unzip routine and make_max_finder come to mind, as well as the details on catching the errant exceptions from WWW::Mechanize.

Thanks also to L<http://www.github.com> for hosting open source repositories for free.

=head1 LICENSE AND COPYRIGHT

Copyright 2015 Michael P. Fabio.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See L<http://dev.perl.org/licenses/> for more information.


=cut
