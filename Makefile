#===============================================================================
# Makefile for FBS (Few-Body System) SVM Calculation
#===============================================================================
#
# This Makefile builds the modern Fortran version of the SVM code.
#
# Usage:
#   make              - Build with LAPACK support (default)
#   make lapack       - Build with LAPACK support (explicit)
#   make openblas     - Build with OpenBLAS support
#   make nolapack     - Build without external libraries
#   make clean        - Remove build files
#   make help         - Show this help
#
# Requirements:
#   - gfortran (GNU Fortran compiler) or ifort (Intel Fortran)
#   - LAPACK/BLAS libraries (for optimal performance)
#
#===============================================================================

#-------------------------------------------------------------------------------
# Compiler settings
#-------------------------------------------------------------------------------
# Use gfortran by default, can override with: make FC=ifort
FC = gfortran

# Compiler flags
# -O3: High optimization
# -march=native: Optimize for local CPU
# -ffast-math: Fast floating point (may reduce precision slightly)
# -cpp: Enable C preprocessor for #ifdef directives
FFLAGS = -O3 -march=native -ffast-math -cpp

# Debug flags (use: make DEBUG=1)
ifdef DEBUG
    FFLAGS = -g -O0 -Wall -Wextra -fcheck=all -fbacktrace -cpp
endif

#-------------------------------------------------------------------------------
# LAPACK/BLAS settings
#-------------------------------------------------------------------------------
# Default: use system LAPACK
LAPACK_FLAGS = -DUSE_LAPACK
LAPACK_LIBS = -llapack -lblas

# macOS Accelerate framework (automatically detected)
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    LAPACK_LIBS = -framework Accelerate
endif

#-------------------------------------------------------------------------------
# Directories
#-------------------------------------------------------------------------------
SRCDIR = src
OBJDIR = obj
BINDIR = .

#-------------------------------------------------------------------------------
# Source files (in dependency order)
#-------------------------------------------------------------------------------
SOURCES = \
    $(SRCDIR)/constants_mod.f90 \
    $(SRCDIR)/parameters_mod.f90 \
    $(SRCDIR)/random_mod.f90 \
    $(SRCDIR)/linear_algebra_mod.f90 \
    $(SRCDIR)/special_functions_mod.f90 \
    $(SRCDIR)/coordinate_transform_mod.f90 \
    $(SRCDIR)/permutation_mod.f90 \
    $(SRCDIR)/potential_mod.f90 \
    $(SRCDIR)/matrix_elements_mod.f90 \
    $(SRCDIR)/diagonalization_mod.f90 \
    $(SRCDIR)/io_mod.f90 \
    $(SRCDIR)/svm_mod.f90 \
    $(SRCDIR)/main.f90

# Object files
OBJECTS = $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SOURCES))

# Module files
MODULES = $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.mod,$(SOURCES))

# Executable name
PROGRAM = fbs

#-------------------------------------------------------------------------------
# Default target: build with LAPACK
#-------------------------------------------------------------------------------
.PHONY: all lapack openblas nolapack clean help

all: lapack

#-------------------------------------------------------------------------------
# Build with LAPACK (default)
#-------------------------------------------------------------------------------
lapack: FFLAGS += $(LAPACK_FLAGS)
lapack: LIBS = $(LAPACK_LIBS)
lapack: $(BINDIR)/$(PROGRAM)
	@echo ""
	@echo "Build complete: $(PROGRAM) (with LAPACK support)"
	@echo ""

#-------------------------------------------------------------------------------
# Build with OpenBLAS
#-------------------------------------------------------------------------------
openblas: FFLAGS += -DUSE_LAPACK
openblas: LIBS = -lopenblas
openblas: $(BINDIR)/$(PROGRAM)
	@echo ""
	@echo "Build complete: $(PROGRAM) (with OpenBLAS support)"
	@echo ""

#-------------------------------------------------------------------------------
# Build without external libraries
#-------------------------------------------------------------------------------
nolapack: LIBS =
nolapack: $(BINDIR)/$(PROGRAM)
	@echo ""
	@echo "Build complete: $(PROGRAM) (without LAPACK)"
	@echo "Note: Using built-in linear algebra routines"
	@echo ""

#-------------------------------------------------------------------------------
# Create object directory
#-------------------------------------------------------------------------------
$(OBJDIR):
	mkdir -p $(OBJDIR)

#-------------------------------------------------------------------------------
# Compile source files to object files
#-------------------------------------------------------------------------------
$(OBJDIR)/%.o: $(SRCDIR)/%.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -c $< -o $@ -J$(OBJDIR)

#-------------------------------------------------------------------------------
# Link object files to executable
#-------------------------------------------------------------------------------
$(BINDIR)/$(PROGRAM): $(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $(OBJECTS) $(LIBS)

#-------------------------------------------------------------------------------
# Module dependencies (order matters!)
#-------------------------------------------------------------------------------
$(OBJDIR)/parameters_mod.o: $(OBJDIR)/constants_mod.o
$(OBJDIR)/random_mod.o: $(OBJDIR)/constants_mod.o
$(OBJDIR)/linear_algebra_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o
$(OBJDIR)/special_functions_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o
$(OBJDIR)/coordinate_transform_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/linear_algebra_mod.o
$(OBJDIR)/permutation_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o
$(OBJDIR)/potential_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/special_functions_mod.o
$(OBJDIR)/matrix_elements_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/linear_algebra_mod.o $(OBJDIR)/potential_mod.o
$(OBJDIR)/diagonalization_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/linear_algebra_mod.o $(OBJDIR)/special_functions_mod.o
$(OBJDIR)/io_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/special_functions_mod.o $(OBJDIR)/potential_mod.o
$(OBJDIR)/svm_mod.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/random_mod.o $(OBJDIR)/linear_algebra_mod.o $(OBJDIR)/coordinate_transform_mod.o $(OBJDIR)/matrix_elements_mod.o $(OBJDIR)/diagonalization_mod.o $(OBJDIR)/io_mod.o
$(OBJDIR)/main.o: $(OBJDIR)/constants_mod.o $(OBJDIR)/parameters_mod.o $(OBJDIR)/io_mod.o $(OBJDIR)/coordinate_transform_mod.o $(OBJDIR)/permutation_mod.o $(OBJDIR)/svm_mod.o

#-------------------------------------------------------------------------------
# Clean build files
#-------------------------------------------------------------------------------
clean:
	rm -rf $(OBJDIR)
	rm -f $(BINDIR)/$(PROGRAM)
	rm -f *.mod
	@echo "Clean complete"

#-------------------------------------------------------------------------------
# Help target
#-------------------------------------------------------------------------------
help:
	@echo ""
	@echo "FBS (Few-Body System) SVM Calculation - Build System"
	@echo "====================================================="
	@echo ""
	@echo "Usage:"
	@echo "  make              - Build with LAPACK support (default)"
	@echo "  make lapack       - Build with LAPACK support (explicit)"
	@echo "  make openblas     - Build with OpenBLAS support"
	@echo "  make nolapack     - Build without external libraries"
	@echo "  make clean        - Remove build files"
	@echo "  make help         - Show this help"
	@echo ""
	@echo "Options:"
	@echo "  FC=ifort          - Use Intel Fortran compiler"
	@echo "  DEBUG=1           - Build with debugging enabled"
	@echo ""
	@echo "Examples:"
	@echo "  make FC=ifort     - Build with Intel Fortran + LAPACK"
	@echo "  make DEBUG=1      - Build debug version with LAPACK"
	@echo ""
	@echo "Output:"
	@echo "  ./fbs             - Executable program"
	@echo ""
	@echo "Input files required:"
	@echo "  fbs.inp           - System parameters"
	@echo "  pot.inp           - Potential parameters"
	@echo ""
