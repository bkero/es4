@echo off

REM The following licensing terms and conditions apply and must be
REM accepted in order to use the Reference Implementation:
REM 
REM    1. This Reference Implementation is made available to all
REM interested persons on the same terms as Ecma makes available its
REM standards and technical reports, as set forth at
REM http://www.ecma-international.org/publications/.
REM 
REM    2. All liability and responsibility for any use of this Reference
REM Implementation rests with the user, and not with any of the parties
REM who contribute to, or who own or hold any copyright in, this Reference
REM Implementation.
REM 
REM    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
REM HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
REM WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
REM MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
REM DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
REM LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
REM CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
REM SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
REM BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
REM WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
REM OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
REM IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REM 
REM End of Terms and Conditions
REM 
REM Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
REM Software ASA, and others.

SET ThisDir=%~dp0
SET Exe=%ThisDir%run.exe
SET Image=%ThisDir%es4.image
SET MLton=

FOR %%A IN (%*) DO (
    IF [%%A]==[-b] GOTO Boot
)

SET MLton=@MLton load-world "%Image%" --

:Boot

CALL "%Exe%" %MLton% %*
