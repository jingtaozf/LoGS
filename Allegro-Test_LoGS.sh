#!/bin/sh

# Logs extensible (common-lisp based) log/event analysis engine/language
# Copyright (C) 2003-2008 James Earl Prewett

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

alisp -I ./LoGS.dxl -e "(progn (push #p\"/Users/dl/.sbcl/systems\" asdf:*central-registry*) (asdf:operate 'asdf:load-op :lift)(asdf:operate 'asdf:load-op :logs-tests))"
RET=$?

echo
if (( $RET )) ; then
echo SOME TESTS FAILED! BAH!
else
echo ALL TESTS PASSED!
fi

