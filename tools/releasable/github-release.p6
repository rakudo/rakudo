#!/usr/bin/env perl6
use Cro::HTTP::Client;

unit sub MAIN(
    Str  :$repo!,         #= like ‘rakudo/rakudo’
    Str  :$tag!,          #= like ‘2019.03’
    Str  :$token!,        #= like ‘a1b2c3d4e5a1b2c3d4e5a1b2c3d4e5a1b2c3d4e5’
    Str  :$announcement,  #= like ‘announce/2019.03.md’
         :asset($assets), #= like ‘rakudo-2019.03.tar.gz’
);

die “Announcement file $announcement does not exist” if $announcement and $announcement.IO.e.not;
die “Asset file $_ does not exist” unless .IO.e for @$assets;

my $url-prefix = ‘https://api.github.com/repos’;
my $headers = [
    User-Agent => ‘Rakudo Perl 6 release automation’,
    Authorization => “token $token”,
];

sub get-release($tag) {
    my $url = “$url-prefix/$repo/releases/tags/$tag”;
    my $resp = await Cro::HTTP::Client.get: $url, :$headers;
    return await $resp.body
}

sub create-release($tag, :$name, :$body) {
    my %body = tag_name => $tag;
    %body ,= :$name with $name;
    %body ,= :$body with $body;

    my $url = “$url-prefix/$repo/releases”;
    my $resp = await Cro::HTTP::Client.post: $url, :$headers,
               :%body, content-type => ‘application/json’;

    return await $resp.body
}

sub edit-release($release-id, :$name, :$body) {
    my %body;
    %body ,= :$name with $name;
    %body ,= :$body with $body;

    my $url = “$url-prefix/$repo/releases/$release-id”;
    my $resp = await Cro::HTTP::Client.patch: $url, :$headers,
               :%body, content-type => ‘application/json’;

    return await $resp.body
}

sub upload-asset($url is copy, IO() $path) {
    $url .= subst: ‘{?name,label}’, ‘’;
    $url ~= ‘?name=’ ~ $path.basename; # TODO urlencode
    my $content-type = run(:out, <file --mime-type -b -->, $path).out.slurp.chomp;
    my $body = $path.slurp: :bin;
    my $resp = await Cro::HTTP::Client.post: $url, :$headers,
                                             :$body, :$content-type;

    return await $resp.body
}

sub githubify($announcement-text) {
    die unless $announcement-text ~~
    /^\s*‘# ’?‘Announce: Rakudo’[‘ Perl’\s‘6’]?‘ compiler’.*?(<[Rr]>‘elease ’\N+)$$\s*
       (.*)$/;
     return %(name => $0.Str.tc, body => ~$1)
}

my $text = %(name => ~$tag, body => ‘’);
$text = githubify $announcement.IO.slurp if $announcement;
my $release = try get-release ~$tag;

if $release {
    note “Editing $tag”;
    edit-release $release<id>, |$text;
} else {
    note “Creating $tag”;
    $release = create-release ~$tag, |$text;
}

my %existing-assets = $release<assets>.map: { .<name> => $_ };
for @$assets -> IO() $_ {
    my $existing-asset = %existing-assets{.basename};
    if $existing-asset {
        if $existing-asset<size> != .s {
            die ‘Size doesn't match, are you trying to replace a file? Don't.’
        }
        say .basename, ‘ is already uploaded’;
        next;
    }
    note “Uploading $_”;
    upload-asset $release<upload_url>, $_
}
