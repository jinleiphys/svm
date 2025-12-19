#===============================================================================
# Makefile for FBS (Few-Body System) SVM Calculation
#===============================================================================
#
# Usage:
#   ./setup.sh        - Configure for your system (run first!)
#   make              - Build the program
#   make clean        - Remove build files
#   make help         - Show this help
#
#===============================================================================

# Include auto-generated configuration
-include make.inc

# Default values if make.inc not found
FC ?= gfortran
FFLAGS ?= -O3 -march=native -ffast-math -cpp -fopenmp
LAPACK_FLAGS ?= -DUSE_LAPACK

# Detect macOS and use Accelerate if LAPACK_LIBS not set
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    LAPACK_LIBS ?= -framework Accelerate
else
    LAPACK_LIBS ?= -llapack -lblas
endif

#-------------------------------------------------------------------------------
# Directories
#-------------------------------------------------------------------------------
SRCDIR = src
OBJDIR = obj
MODDIR = obj

#-------------------------------------------------------------------------------
# Source files (in compilation order - dependencies matter!)
#-------------------------------------------------------------------------------
SRCS = \
    constants_mod.f90 \
    parameters_mod.f90 \
    random_mod.f90 \
    linear_algebra_mod.f90 \
    special_functions_mod.f90 \
    coordinate_transform_mod.f90 \
    permutation_mod.f90 \
    potential_mod.f90 \
    matrix_elements_mod.f90 \
    diagonalization_mod.f90 \
    io_mod.f90 \
    svm_mod.f90 \
    main.f90

# Full paths
SOURCES = $(addprefix $(SRCDIR)/,$(SRCS))
OBJECTS = $(addprefix $(OBJDIR)/,$(SRCS:.f90=.o))

# Executable name
PROGRAM = fbs

#-------------------------------------------------------------------------------
# Targets
#-------------------------------------------------------------------------------
.PHONY: all clean help rebuild

all: $(PROGRAM)
	@echo ""
	@echo "Build complete: $(PROGRAM)"
	@echo "LAPACK: $(LAPACK_LIBS)"
	@echo ""

rebuild: clean all

#-------------------------------------------------------------------------------
# Create directories
#-------------------------------------------------------------------------------
$(OBJDIR):
	@mkdir -p $(OBJDIR)

#-------------------------------------------------------------------------------
# Main executable
#-------------------------------------------------------------------------------
$(PROGRAM): $(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $^ $(LAPACK_LIBS)

#-------------------------------------------------------------------------------
# Compilation rules with explicit dependencies
# Each .o depends on its .f90 source AND all module files it uses
#-------------------------------------------------------------------------------

$(OBJDIR)/constants_mod.o: $(SRCDIR)/constants_mod.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR)

$(OBJDIR)/parameters_mod.o: $(SRCDIR)/parameters_mod.f90 \
    $(OBJDIR)/constants_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/random_mod.o: $(SRCDIR)/random_mod.f90 \
    $(OBJDIR)/constants_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/linear_algebra_mod.o: $(SRCDIR)/linear_algebra_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/special_functions_mod.o: $(SRCDIR)/special_functions_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/coordinate_transform_mod.o: $(SRCDIR)/coordinate_transform_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/linear_algebra_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/permutation_mod.o: $(SRCDIR)/permutation_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/potential_mod.o: $(SRCDIR)/potential_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/special_functions_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/matrix_elements_mod.o: $(SRCDIR)/matrix_elements_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/linear_algebra_mod.o $(OBJDIR)/potential_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/diagonalization_mod.o: $(SRCDIR)/diagonalization_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/linear_algebra_mod.o $(OBJDIR)/special_functions_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/io_mod.o: $(SRCDIR)/io_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/special_functions_mod.o $(OBJDIR)/potential_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/svm_mod.o: $(SRCDIR)/svm_mod.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/random_mod.o \
    $(OBJDIR)/linear_algebra_mod.o $(OBJDIR)/coordinate_transform_mod.o \
    $(OBJDIR)/matrix_elements_mod.o $(OBJDIR)/diagonalization_mod.o $(OBJDIR)/io_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

$(OBJDIR)/main.o: $(SRCDIR)/main.f90 \
    $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/io_mod.o \
    $(OBJDIR)/coordinate_transform_mod.o $(OBJDIR)/permutation_mod.o $(OBJDIR)/svm_mod.o | $(OBJDIR)
	$(FC) $(FFLAGS) $(LAPACK_FLAGS) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

#-------------------------------------------------------------------------------
# Clean
#-------------------------------------------------------------------------------
clean:
	rm -rf $(OBJDIR) $(PROGRAM) *.mod
	@echo "Clean complete"

#-------------------------------------------------------------------------------
# Help
#-------------------------------------------------------------------------------
help:
	@echo ""
	@echo "FBS (Few-Body System) SVM Calculation"
	@echo "======================================"
	@echo ""
	@echo "Setup:"
	@echo "  ./setup.sh        - Auto-detect and configure BLAS/LAPACK"
	@echo ""
	@echo "Build:"
	@echo "  make              - Build the program"
	@echo "  make rebuild      - Clean and rebuild"
	@echo "  make clean        - Remove build files"
	@echo "  make help         - Show this help"
	@echo ""
