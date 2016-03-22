#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;
use FindBin;
use Log::Any::Adapter 'TAP';

use ELF::Writer;
use ELF::Writer::Linux_x86_64;

subtest enums => \&test_enums;
sub test_enums {
	my $elf;
	
	for (qw: executable shared relocatable core :) {
		$elf= ELF::Writer->new(type => $_);
		is( $elf->type, $_, "enum type=$_ decoded" );
		is( $elf->type_num, $ELF::Writer::TypeEnum_r{$_}, "enum type=$_ correct value" );
	}
	$elf= ELF::Writer->new(type => 42);
	is( $elf->type_num, 42, "enum type=42 allowed" );
	is( $elf->type, 42, "enum type=42 decoded as self" );
	
	for (qw: 32bit 64bit :) {
		$elf= ELF::Writer->new(class => $_);
		is( $elf->class, $_, "enum class=$_ decoded" );
		is( $elf->class_num, $ELF::Writer::ClassEnum_r{$_}, "enum class=$_ correct value" );
	}
	
	for (qw: 2LSB 2MSB :) {
		$elf= ELF::Writer->new(data => $_);
		is( $elf->data, $_, "enum data=$_ decoded" );
		is( $elf->data_num, $ELF::Writer::DataEnum_r{$_}, "enum data=$_ correct value" );
	}
	
	for (qw: Linux Solaris :) {
		$elf= ELF::Writer->new(osabi => $_);
		is( $elf->osabi, $_, "enum osabi=$_ decoded" );
		is( $elf->osabi_num, $ELF::Writer::OsabiEnum_r{$_}, "enum osabi=$_ correct value" );
	}
	
	for (qw: x86-64 :) {
		$elf= ELF::Writer->new(machine => $_);
		is( $elf->machine, $_, "enum machine=$_ decoded" );
		is( $elf->machine_num, $ELF::Writer::MachineEnum_r{$_}, "enum machine=$_ correct value" );
	}
	
	for (qw: note :) {
		my $seg= ELF::Writer::Segment->new(type => $_);
		is( $seg->type, $_, "enum segment.type=$_ decoded" );
		is( $seg->type_num, $ELF::Writer::SegmentTypeEnum_r{$_}, "enum segment.type=$_ correct value" );
	}
};

subtest simple_x86_64_elf => \&test_return_42;
sub test_return_42 {
	my $elf= ELF::Writer::Linux_x86_64->new(
		type => 'executable',
		segments => [{
			offset      => 0, # overlap segment with elf header
			virt_addr   => 0x10000,
			data        => "\xbf\x2a\x00\x00\x00\xb8\x3c\x00\x00\x00\x0f\x05",
			data_start  => undef # to be calculated below
		}],
	);
	
	my $prog_offset= $elf->elf_header_len + $elf->segment_header_elem_len;
	$elf->segments->[0]->data_start( $prog_offset );
	$elf->entry_point( $elf->segments->[0]->virt_addr + $prog_offset );
	
	# Write out an elf file
	$elf= $elf->serialize();
	my $expected= _slurp("$FindBin::Bin/data/return_42_Linux_x86_64");
	is( $elf, $expected, 'file contents match' )
		or do {
			diag join ' ', map { sprintf("%02x", ord($_)) } split //, $elf;
			diag join ' ', map { sprintf("%02x", ord($_)) } split //, $expected;
		};
}

sub _slurp { open my $fh, '<:raw', shift or die; $/= undef; <$fh> }

done_testing;
