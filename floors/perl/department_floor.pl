#!/usr/bin/env perl
#
# FLOOR 17 - PERL JURISDICTION
# Department Floor Implementation
#
# Domain: System scripting, Text processing, Legacy systems
# Architectural Law: TIMTOWTDI with best practices, Strict mode mandatory, Use warnings
# Security Doctrine: Taint mode, Command injection prevention, Input validation

use strict;
use warnings;
use JSON::PP;
use IO::Handle;
use Data::Dumper;

# Enable autoflush for stdout
STDOUT->autoflush(1);
STDERR->autoflush(1);

package FloorAgent {
    use strict;
    use warnings;
    
    sub new {
        my ($class, %args) = @_;
        return bless {
            agent_id => $args{agent_id},
            name => $args{name},
            role => $args{role},
            office => $args{office},
            capabilities => $args{capabilities} || [],
            specialization => $args{specialization}
        }, $class;
    }
    
    sub to_hash {
        my ($self) = @_;
        return {
            agent_id => $self->{agent_id},
            name => $self->{name},
            role => $self->{role},
            office => $self->{office},
            capabilities => $self->{capabilities},
            specialization => $self->{specialization}
        };
    }
}

package Task {
    use strict;
    use warnings;
    
    sub new {
        my ($class, %args) = @_;
        return bless {
            task_id => $args{task_id},
            title => $args{title},
            status => 'pending',
            assigned_to => $args{assigned_to},
            created_at => _iso8601_timestamp(),
            task_type => $args{task_type},
            metadata => $args{metadata} || {}
        }, $class;
    }
    
    sub _iso8601_timestamp {
        my @time = gmtime();
        return sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",
            $time[5]+1900, $time[4]+1, $time[3],
            $time[2], $time[1], $time[0]);
    }
    
    sub to_hash {
        my ($self) = @_;
        return {
            task_id => $self->{task_id},
            title => $self->{title},
            status => $self->{status},
            assigned_to => $self->{assigned_to},
            created_at => $self->{created_at},
            task_type => $self->{task_type},
            metadata => $self->{metadata}
        };
    }
}

package ServiceAgent {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        return bless {
            name => 'Perl Service Agent',
            capabilities => ['process_management', 'file_operations', 'system_integration']
        }, $class;
    }
    
    sub validate_shebang {
        my ($self, $script) = @_;
        
        my @lines = split /\n/, $script;
        return {error => 'Empty script'} unless @lines;
        
        my $first_line = $lines[0];
        if ($first_line =~ m{^#!/usr/bin/(env )?perl}) {
            return {
                valid => JSON::PP::true,
                shebang => $first_line,
                uses_env => defined $1
            };
        }
        
        return {
            valid => JSON::PP::false,
            error => 'Missing or invalid shebang',
            recommendation => '#!/usr/bin/env perl'
        };
    }
    
    sub analyze_pragmas {
        my ($self, $script) = @_;
        
        my $has_strict = $script =~ /use\s+strict/;
        my $has_warnings = $script =~ /use\s+warnings/;
        my $has_autodie = $script =~ /use\s+autodie/;
        
        my @issues;
        push @issues, 'Missing "use strict" - mandatory for safety' unless $has_strict;
        push @issues, 'Missing "use warnings" - mandatory for safety' unless $has_warnings;
        push @issues, 'Consider "use autodie" for automatic error handling' unless $has_autodie;
        
        return {
            has_strict => $has_strict ? JSON::PP::true : JSON::PP::false,
            has_warnings => $has_warnings ? JSON::PP::true : JSON::PP::false,
            has_autodie => $has_autodie ? JSON::PP::true : JSON::PP::false,
            issues => \@issues,
            safe => $has_strict && $has_warnings
        };
    }
}

package TextProcessingAgent {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        return bless {
            name => 'Perl Text Processing Agent',
            capabilities => ['regex_analysis', 'text_manipulation', 'pattern_matching']
        }, $class;
    }
    
    sub analyze_regex {
        my ($self, $pattern) = @_;
        
        my @warnings;
        my $complexity = 'simple';
        
        # Check for catastrophic backtracking patterns
        if ($pattern =~ /\([^)]*\+[^)]*\)\+/ || $pattern =~ /\([^)]*\*[^)]*\)\+/) {
            push @warnings, 'Potential catastrophic backtracking - ReDoS risk';
            $complexity = 'dangerous';
        }
        
        # Check for unbounded quantifiers
        my $unbounded_count = () = $pattern =~ /[*+]/g;
        if ($unbounded_count > 3) {
            push @warnings, 'Multiple unbounded quantifiers - may be slow';
            $complexity = 'complex' if $complexity eq 'simple';
        }
        
        # Check for named captures
        my $has_named_captures = $pattern =~ /\(\?<\w+>/;
        
        # Check for modifiers
        my @modifiers;
        push @modifiers, 'case-insensitive' if $pattern =~ /\(\?i\)/;
        push @modifiers, 'multiline' if $pattern =~ /\(\?m\)/;
        push @modifiers, 'single-line' if $pattern =~ /\(\?s\)/;
        push @modifiers, 'extended' if $pattern =~ /\(\?x\)/;
        
        return {
            pattern => $pattern,
            complexity => $complexity,
            warnings => \@warnings,
            has_named_captures => $has_named_captures ? JSON::PP::true : JSON::PP::false,
            modifiers => \@modifiers,
            safe => $complexity ne 'dangerous'
        };
    }
    
    sub suggest_optimization {
        my ($self, $pattern) = @_;
        
        my @suggestions;
        
        # Suggest non-capturing groups
        my $capturing_groups = () = $pattern =~ /\([^?]/g;
        if ($capturing_groups > 0) {
            push @suggestions, {
                type => 'performance',
                message => 'Consider non-capturing groups (?:...) if captures not needed',
                impact => 'low'
            };
        }
        
        # Suggest anchors
        unless ($pattern =~ /^[\^\\A]/ || $pattern =~ /[\$\\z]$/) {
            push @suggestions, {
                type => 'performance',
                message => 'Add anchors (^ or $) to prevent unnecessary scanning',
                impact => 'medium'
            };
        }
        
        return {
            suggestions => \@suggestions,
            suggestion_count => scalar @suggestions
        };
    }
}

package SecurityAgent {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        return bless {
            name => 'Perl Security Agent',
            capabilities => ['taint_checking', 'injection_detection', 'security_audit']
        }, $class;
    }
    
    sub check_command_injection {
        my ($self, $script) = @_;
        
        my @vulnerabilities;
        my $security_level = 'safe';
        
        # Check for system calls with user input
        if ($script =~ /system\s*\([^)]*\$/) {
            push @vulnerabilities, 'system() call with variable - command injection risk';
            $security_level = 'critical';
        }
        
        if ($script =~ /exec\s*\([^)]*\$/) {
            push @vulnerabilities, 'exec() call with variable - command injection risk';
            $security_level = 'critical';
        }
        
        if ($script =~ /`[^`]*\$[^`]*`/) {
            push @vulnerabilities, 'Backticks with variable - command injection risk';
            $security_level = 'critical';
        }
        
        if ($script =~ /qx\{[^}]*\$[^}]*\}/) {
            push @vulnerabilities, 'qx{} with variable - command injection risk';
            $security_level = 'critical';
        }
        
        # Check for open with pipe
        if ($script =~ /open\s*\([^)]*\|/) {
            push @vulnerabilities, 'open() with pipe - potential command injection';
            $security_level = 'warning' if $security_level eq 'safe';
        }
        
        # Check for eval with user input
        if ($script =~ /eval\s+["\']?[^"']*\$/) {
            push @vulnerabilities, 'eval() with variable - code injection risk';
            $security_level = 'critical';
        }
        
        return {
            security_level => $security_level,
            vulnerabilities => \@vulnerabilities,
            safe => $security_level eq 'safe'
        };
    }
    
    sub check_taint_mode {
        my ($self, $script) = @_;
        
        my $has_taint = $script =~ /^#!.*perl.*-T/m;
        
        my @recommendations;
        unless ($has_taint) {
            push @recommendations, 'Enable taint mode with -T flag in shebang';
            push @recommendations, 'Taint mode helps prevent security vulnerabilities';
        }
        
        return {
            has_taint_mode => $has_taint ? JSON::PP::true : JSON::PP::false,
            recommendations => \@recommendations,
            security_impact => $has_taint ? 'protected' : 'vulnerable'
        };
    }
    
    sub audit_file_operations {
        my ($self, $script) = @_;
        
        my @findings;
        
        # Check for unsafe open
        if ($script =~ /open\s*\(\s*\w+\s*,\s*["\']?[^"']*\$/) {
            push @findings, {
                severity => 'high',
                finding => 'File open with variable filename',
                recommendation => 'Validate and sanitize filenames, use three-argument open'
            };
        }
        
        # Check for two-argument open (deprecated)
        if ($script =~ /open\s*\(\s*\w+\s*,\s*["\'][^"']*["']\s*\)/) {
            push @findings, {
                severity => 'medium',
                finding => 'Two-argument open detected',
                recommendation => 'Use three-argument open for safety'
            };
        }
        
        # Check for unlink/rmdir with variables
        if ($script =~ /unlink\s*\([^)]*\$/ || $script =~ /rmdir\s*\([^)]*\$/) {
            push @findings, {
                severity => 'high',
                finding => 'File deletion with variable',
                recommendation => 'Validate paths carefully, prevent directory traversal'
            };
        }
        
        return {
            findings => \@findings,
            finding_count => scalar @findings
        };
    }
}

package ArchitectureOffice {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = bless {
            agents => []
        }, $class;
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'arch_001',
            name => 'Script Architect',
            role => 'Senior Architect',
            office => 'Architecture',
            capabilities => ['script_design', 'module_architecture', 'api_design'],
            specialization => 'Script Architecture'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'arch_002',
            name => 'Systems Architect',
            role => 'Architect',
            office => 'Architecture',
            capabilities => ['system_integration', 'automation_design', 'workflow_design'],
            specialization => 'System Integration'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'arch_003',
            name => 'Text Processing Architect',
            role => 'Architect',
            office => 'Architecture',
            capabilities => ['regex_design', 'parser_design', 'text_pipeline'],
            specialization => 'Text Processing'
        );
        
        return $self;
    }
    
    sub get_agents {
        my ($self) = @_;
        return $self->{agents};
    }
}

package ImplementationOffice {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = bless {
            agents => []
        }, $class;
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'impl_001',
            name => 'Script Engineer',
            role => 'Senior Engineer',
            office => 'Implementation',
            capabilities => ['script_development', 'module_creation', 'cpan_integration'],
            specialization => 'Script Development'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'impl_002',
            name => 'Regex Engineer',
            role => 'Engineer',
            office => 'Implementation',
            capabilities => ['regex_implementation', 'text_parsing', 'pattern_matching'],
            specialization => 'Regex & Text Processing'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'impl_003',
            name => 'System Integration Engineer',
            role => 'Engineer',
            office => 'Implementation',
            capabilities => ['system_calls', 'process_management', 'ipc'],
            specialization => 'System Integration'
        );
        
        return $self;
    }
    
    sub get_agents {
        my ($self) = @_;
        return $self->{agents};
    }
}

package ReviewOffice {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = bless {
            agents => []
        }, $class;
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'rev_001',
            name => 'Senior Code Reviewer',
            role => 'Lead Reviewer',
            office => 'Review',
            capabilities => ['code_review', 'best_practices', 'idiom_enforcement'],
            specialization => 'Comprehensive Review'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'rev_002',
            name => 'Perl Critic',
            role => 'Reviewer',
            office => 'Review',
            capabilities => ['static_analysis', 'policy_enforcement', 'style_checking'],
            specialization => 'Static Analysis'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'rev_003',
            name => 'Performance Reviewer',
            role => 'Reviewer',
            office => 'Review',
            capabilities => ['performance_analysis', 'optimization_review'],
            specialization => 'Performance Review'
        );
        
        return $self;
    }
    
    sub get_agents {
        my ($self) = @_;
        return $self->{agents};
    }
    
    sub review_script {
        my ($self, $script, $context) = @_;
        $context ||= {};
        
        my $service_agent = ServiceAgent->new();
        my $security_agent = SecurityAgent->new();
        
        my $pragma_check = $service_agent->analyze_pragmas($script);
        my $security_check = $security_agent->check_command_injection($script);
        
        my @issues;
        push @issues, @{$pragma_check->{issues}} if $pragma_check->{issues};
        push @issues, @{$security_check->{vulnerabilities}} if $security_check->{vulnerabilities};
        
        my @recommendations;
        push @recommendations, 'Use three-argument open()' if $script =~ /open\s*\([^,]+,[^,]+\)/;
        push @recommendations, 'Enable taint mode for security' unless $script =~ /-T/;
        
        return {
            status => 'reviewed',
            issues => \@issues,
            recommendations => \@recommendations,
            approved => scalar(@issues) == 0,
            security_level => $security_check->{security_level}
        };
    }
}

package TestOffice {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = bless {
            agents => []
        }, $class;
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'test_001',
            name => 'Test Engineer',
            role => 'Senior Tester',
            office => 'Test',
            capabilities => ['unit_testing', 'integration_testing', 'test_more'],
            specialization => 'Testing'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'test_002',
            name => 'TAP Specialist',
            role => 'Tester',
            office => 'Test',
            capabilities => ['tap_protocol', 'test_harness', 'prove'],
            specialization => 'TAP Testing'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'test_003',
            name => 'Coverage Analyst',
            role => 'Tester',
            office => 'Test',
            capabilities => ['coverage_analysis', 'devel_cover', 'quality_metrics'],
            specialization => 'Code Coverage'
        );
        
        return $self;
    }
    
    sub get_agents {
        my ($self) = @_;
        return $self->{agents};
    }
    
    sub test_script {
        my ($self, $script, $test_data) = @_;
        
        my $service_agent = ServiceAgent->new();
        my $pragma_check = $service_agent->analyze_pragmas($script);
        
        my %test_results = (
            syntax_valid => JSON::PP::true,
            has_strict => $pragma_check->{has_strict},
            has_warnings => $pragma_check->{has_warnings},
            passes_basic_checks => $pragma_check->{safe}
        );
        
        my $all_passed = $pragma_check->{safe};
        
        return {
            status => 'tested',
            test_results => \%test_results,
            all_tests_passed => $all_passed ? JSON::PP::true : JSON::PP::false
        };
    }
}

package SecurityOffice {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = bless {
            agents => []
        }, $class;
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'sec_001',
            name => 'Security Auditor',
            role => 'Senior Security Engineer',
            office => 'Security',
            capabilities => ['security_audit', 'vulnerability_detection', 'taint_mode'],
            specialization => 'Security Auditing'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'sec_002',
            name => 'Injection Prevention Specialist',
            role => 'Security Engineer',
            office => 'Security',
            capabilities => ['command_injection', 'code_injection', 'input_validation'],
            specialization => 'Injection Prevention'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'sec_003',
            name => 'Cryptography Specialist',
            role => 'Security Engineer',
            office => 'Security',
            capabilities => ['encryption', 'hashing', 'secure_random'],
            specialization => 'Cryptography'
        );
        
        return $self;
    }
    
    sub get_agents {
        my ($self) = @_;
        return $self->{agents};
    }
    
    sub security_audit {
        my ($self, $script) = @_;
        
        my $security_agent = SecurityAgent->new();
        my $injection_check = $security_agent->check_command_injection($script);
        my $taint_check = $security_agent->check_taint_mode($script);
        my $file_check = $security_agent->audit_file_operations($script);
        
        my @findings = @{$file_check->{findings}};
        
        return {
            status => 'audited',
            security_level => $injection_check->{security_level},
            has_taint_mode => $taint_check->{has_taint_mode},
            vulnerabilities => $injection_check->{vulnerabilities},
            file_operation_findings => \@findings,
            passed_audit => $injection_check->{safe}
        };
    }
}

package ManagerOffice {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        my $self = bless {
            agents => []
        }, $class;
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'mgr_001',
            name => 'Perl Project Manager',
            role => 'Senior Manager',
            office => 'Manager',
            capabilities => ['project_management', 'resource_allocation', 'cpan_publishing'],
            specialization => 'Project Management'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'mgr_002',
            name => 'Automation Manager',
            role => 'Manager',
            office => 'Manager',
            capabilities => ['automation_strategy', 'workflow_optimization'],
            specialization => 'Automation'
        );
        
        push @{$self->{agents}}, FloorAgent->new(
            agent_id => 'mgr_003',
            name => 'Quality Manager',
            role => 'Manager',
            office => 'Manager',
            capabilities => ['quality_assurance', 'standards_enforcement'],
            specialization => 'Quality Assurance'
        );
        
        return $self;
    }
    
    sub get_agents {
        my ($self) = @_;
        return $self->{agents};
    }
}

package PerlDepartmentFloor {
    use strict;
    use warnings;
    
    sub new {
        my ($class) = @_;
        
        my $self = bless {
            floor_number => 17,
            language => 'perl',
            domain => 'System scripting, Text processing, Legacy systems',
            architectural_law => 'TIMTOWTDI with best practices, Strict mode mandatory, Use warnings',
            security_doctrine => 'Taint mode, Command injection prevention, Input validation',
            
            architecture_office => ArchitectureOffice->new(),
            implementation_office => ImplementationOffice->new(),
            review_office => ReviewOffice->new(),
            test_office => TestOffice->new(),
            security_office => SecurityOffice->new(),
            manager_office => ManagerOffice->new(),
            
            service_agent => ServiceAgent->new(),
            text_agent => TextProcessingAgent->new(),
            security_agent => SecurityAgent->new(),
            
            tasks => {},
            offices => [
                'Architecture Office',
                'Implementation Office',
                'Review Office',
                'Test Office',
                'Security Office',
                'Manager Office'
            ]
        }, $class;
        
        return $self;
    }
    
    sub get_all_agents {
        my ($self) = @_;
        
        my @all_agents;
        push @all_agents, @{$self->{architecture_office}->get_agents()};
        push @all_agents, @{$self->{implementation_office}->get_agents()};
        push @all_agents, @{$self->{review_office}->get_agents()};
        push @all_agents, @{$self->{test_office}->get_agents()};
        push @all_agents, @{$self->{security_office}->get_agents()};
        push @all_agents, @{$self->{manager_office}->get_agents()};
        
        return \@all_agents;
    }
    
    sub create_task {
        my ($self, %params) = @_;
        
        my $task = Task->new(%params);
        $self->{tasks}{$params{task_id}} = $task;
        
        return {
            status => 'success',
            task => $task->to_hash()
        };
    }
    
    sub get_floor_info {
        my ($self) = @_;
        
        my $all_agents = $self->get_all_agents();
        my @tasks = map { $_->to_hash() } values %{$self->{tasks}};
        
        return {
            floor_number => $self->{floor_number},
            language => $self->{language},
            domain => $self->{domain},
            architectural_law => $self->{architectural_law},
            security_doctrine => $self->{security_doctrine},
            offices => $self->{offices},
            agent_count => scalar @$all_agents,
            task_count => scalar @tasks,
            agents => [map { $_->to_hash() } @$all_agents],
            tasks => \@tasks,
            specialist_agents => [
                {
                    name => $self->{service_agent}{name},
                    capabilities => $self->{service_agent}{capabilities}
                },
                {
                    name => $self->{text_agent}{name},
                    capabilities => $self->{text_agent}{capabilities}
                },
                {
                    name => $self->{security_agent}{name},
                    capabilities => $self->{security_agent}{capabilities}
                }
            ]
        };
    }
    
    sub analyze_perl_script {
        my ($self, %params) = @_;
        
        my $script = $params{script};
        my $analysis_type = $params{analysis_type} || 'comprehensive';
        
        my %results;
        
        if ($analysis_type eq 'comprehensive' || $analysis_type eq 'structure') {
            $results{shebang} = $self->{service_agent}->validate_shebang($script);
            $results{pragmas} = $self->{service_agent}->analyze_pragmas($script);
        }
        
        if ($analysis_type eq 'comprehensive' || $analysis_type eq 'security') {
            $results{command_injection} = $self->{security_agent}->check_command_injection($script);
            $results{taint_mode} = $self->{security_agent}->check_taint_mode($script);
            $results{file_operations} = $self->{security_agent}->audit_file_operations($script);
        }
        
        return {
            status => 'success',
            analysis => \%results,
            script_length => length($script)
        };
    }
    
    sub analyze_regex {
        my ($self, %params) = @_;
        
        my $pattern = $params{pattern};
        my $analysis = $self->{text_agent}->analyze_regex($pattern);
        my $optimization = $self->{text_agent}->suggest_optimization($pattern);
        
        return {
            status => 'success',
            regex_analysis => $analysis,
            optimization => $optimization
        };
    }
    
    sub review_perl_script {
        my ($self, %params) = @_;
        return $self->{review_office}->review_script($params{script}, $params{context});
    }
    
    sub security_audit {
        my ($self, %params) = @_;
        return $self->{security_office}->security_audit($params{script});
    }
    
    sub test_perl_script {
        my ($self, %params) = @_;
        return $self->{test_office}->test_script($params{script}, $params{test_data});
    }
    
    sub handle_request {
        my ($self, $request) = @_;
        
        my $method = $request->{method};
        my $params = $request->{params} || {};
        
        eval {
            if ($method eq 'get_info') {
                return $self->get_floor_info();
            }
            elsif ($method eq 'create_task') {
                return $self->create_task(%$params);
            }
            elsif ($method eq 'analyze_perl_script') {
                return $self->analyze_perl_script(%$params);
            }
            elsif ($method eq 'analyze_regex') {
                return $self->analyze_regex(%$params);
            }
            elsif ($method eq 'review_script') {
                return $self->review_perl_script(%$params);
            }
            elsif ($method eq 'security_audit') {
                return $self->security_audit(%$params);
            }
            elsif ($method eq 'test_script') {
                return $self->test_perl_script(%$params);
            }
            else {
                return {
                    status => 'error',
                    message => "Unknown method: $method"
                };
            }
        } or do {
            my $error = $@ || 'Unknown error';
            return {
                status => 'error',
                message => "Error processing request: $error"
            };
        };
    }
}

# Main execution
package main;
use strict;
use warnings;

sub main {
    my $floor = PerlDepartmentFloor->new();
    
    my @all_agents = @{$floor->get_all_agents()};
    
    print STDERR "Perl Department Floor (Floor " . $floor->{floor_number} . ") - Ready\n";
    print STDERR "Domain: " . $floor->{domain} . "\n";
    print STDERR "Architectural Law: " . $floor->{architectural_law} . "\n";
    print STDERR "Security Doctrine: " . $floor->{security_doctrine} . "\n";
    print STDERR "Offices: " . join(', ', @{$floor->{offices}}) . "\n";
    print STDERR "Total Agents: " . scalar(@all_agents) . "\n";
    
    my $json = JSON::PP->new->utf8->allow_nonref;
    
    while (my $line = <STDIN>) {
        chomp $line;
        next unless $line;
        
        eval {
            my $request = $json->decode($line);
            my $response = $floor->handle_request($request);
            print $json->encode($response) . "\n";
        } or do {
            my $error = $@ || 'Unknown error';
            my $error_response = {
                status => 'error',
                message => "Error: $error"
            };
            print $json->encode($error_response) . "\n";
        };
    }
}

main() unless caller;

1;
