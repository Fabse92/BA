% LuaTeX class for Supercomputing Frontiers and Innovations.
% Contact superfri.org for questions and proposals.

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{superfri}
\LoadClass[twoside, 11pt, a4paper]{article}

\usepackage[english]{babel}
%\usepackage[unicode=true,linktocpage=true]{hyperref}

%\usepackage{hyperref}
\usepackage{url}


% --- font size ---
\renewcommand{\Large}{\fontsize{14pt}{19.6pt}\selectfont} % title, section
\renewcommand{\large}{\fontsize{12pt}{16.8pt}\selectfont} % authors, subsection, subsubsection
\renewcommand{\footnotesize}{\fontsize{9pt}{12.6pt}\selectfont} % abstract, footnotes
\renewcommand{\normalsize}{\fontsize{11pt}{15.4pt}\selectfont} % text, captions, bibliography, thanx
\renewcommand{\small}{\fontsize{10pt}{14pt}\selectfont} % code

% --- counting different objects ---
\usepackage{totcount}
\regtotcounter{table}
\regtotcounter{figure}

\usepackage{graphicx, epstopdf}
\graphicspath{{pic/}}

\usepackage{indentfirst}

\usepackage{enumitem, changepage, fancyvrb}

% --- space between cite elements ---
\usepackage[space]{cite}

%\usepackage{amsfonts, amsmath, amssymb, amsthm}
%\usepackage{showframe} % show all margins for debugging

% --- enumeration with reduced separation ---
\let\oldenumerate\enumerate
\let\endoldenumerate\endenumerate
\def\enumerate{% FIXME: add support of optional arguments
	\oldenumerate[topsep=0pt, itemsep=0pt, parsep=0pt, widest=9, leftmargin=7mm]
}
\def\enumerateparen{% FIXME: delete this one if the trick with enumerate works
	\oldenumerate[topsep=0pt, itemsep=0pt, parsep=0pt, label=\arabic*), widest=9, leftmargin=7mm]
}
\let\endenumerateparen\endoldenumerate

% --- article classification ---
\let\@classification\@empty
\def\classify#1{\def\@classification{#1}}

% --- authors ---
\renewcommand{\and}{%
	\gdef\multi@uthor{}%
	\leavevmode%
	\unskip, %
}
\newcommand{\authorsplural}{%
	\ifx \multi@uthor \undefined%
		Author%
	\else%
		Authors%
	\fi{}%
}

% --- title ---
\renewcommand{\maketitle}{%
	\ifx \@classification \@empty% 
		\vspace{10pt} % empty line instead of empty classification
	\else%
		{\large{\begin{flushleft}\textbf{\@classification}\end{flushleft}}}%
	\fi%
	{\noindent\Large\raggedright\textbf{\@title}\par}% the title itself
	{\noindent{\begin{flushleft}\large\textbf{\textit{\@author}}\end{flushleft}}}% authors
	{\noindent\footnotesize\copyright{} The \authorsplural{} \the\year. This paper is published with open access at SuperFri.org}
	\vskip 12pt%
}

% --- section numbering ---
\renewcommand{\@seccntformat}[1]{\csname the#1\endcsname.\hspace{0.7em}}

% --- disabling widows and orphans ---
\widowpenalty=10000 % on the first line of a paragraph
\clubpenalty=10000 % on the last line of a paragraph

\usepackage{ifthen,latexsym}
  
\normalsize
\usepackage[margin=2.5cm]{geometry}

% --- itemize with reduced item separation ---
\let\olditemize\itemize
\let\endolditemize\enditemize
\def\itemize{
	\olditemize[topsep=0pt, itemsep=0pt, parsep=0pt]
}

% --- paragraph parameters ---
\parindent=7mm
\parskip=0mm
\leftskip=0mm
\rightskip=0mm
 
% --- vertical skips in display and inline equations ---
\abovedisplayskip=12pt plus 3pt minus 3pt
\belowdisplayskip=12pt plus 3pt minus 3pt
\abovedisplayshortskip=0pt plus 3pt
\belowdisplayshortskip=6pt plus 3pt

\pagestyle{empty}

% --- floats parameters ---
\setcounter{totalnumber}{4}
\setcounter{topnumber}{4}
\setcounter{bottomnumber}{4}
\renewcommand{\textfraction}{0.01}
\renewcommand{\topfraction}{0.99}
\renewcommand{\bottomfraction}{0.99}

\newcommand{\sectspacm}{\hspace*{-\medskipamount}}
\newcommand{\sectspacp}{\hspace*{\medskipamount}}

\renewcommand{\section}{%
	\@startsection{section}{1}{0mm}{-\baselineskip}{0.5\baselineskip}{\normalfont\raggedright\Large\bf}%
}

\renewcommand{\subsection}{%
	\@startsection{subsection}{2}{0mm}{-\baselineskip}{0.5\baselineskip}{\normalfont\raggedright\large\bf}%
}

\renewcommand{\subsubsection}{%
	\@startsection{subsubsection}{3}{0mm}{-\baselineskip}{0.5\baselineskip}{\normalfont\raggedright\large\it}%
}

% --- theorem ---
\newcounter{theocounter}
\newenvironment{theorem}{%
	\refstepcounter{theocounter}\par\addvspace{3mm}\noindent%
	{\bf Theorem \thetheocounter.\ }%
}{%
	\par\addvspace{3mm}%
}

% --- unnumbered theorem ---
\newenvironment{theorem*}{%
	\refstepcounter{theocounter}\par\addvspace{3mm}\noindent%
	{\bf Theorem.\ }\begin{it}%
}{%
	\end{it}\par\addvspace{3mm}
}

% --- lemma ---
\newcounter{lemcounter}
\newenvironment{lemma}{%
	\refstepcounter{lemcounter}\par\addvspace{3mm}\noindent
	{\bf Lemma \thelemcounter.\ }%
}{%
	\par\addvspace{3mm}%
}

% --- remark ---
\newcounter{remcounter}
\newenvironment{remark}{%
	\refstepcounter{remcounter}\par\addvspace{3mm}\noindent
	{\bf Remark \theremcounter.\ }%
}{%
	\par\addvspace{3mm}%
}

% --- definition ---
\newcounter{defcounter}
\newenvironment{definition}{%
	\refstepcounter{defcounter}\par\addvspace{3mm}\noindent
	{\bf Definition \thedefcounter.\ }%
}{%
	\par\addvspace{3mm}%
}

% --- example ---
\newcounter{examcounter}
\newenvironment{example}{%
	\refstepcounter{examcounter}\par\addvspace{3mm}\noindent
	{\bf Example \theexamcounter.\ }%
}{%
	\par\addvspace{3mm}%
}

% --- proposition ---
\newcounter{propcounter}
\newenvironment{proposition}{%
	\refstepcounter{propcounter}\par\addvspace{3mm}\noindent
	{\bf Proposition \thepropcounter.\ }%
}{%
	\par\addvspace{3mm}%
}

% --- corollary ---
\newcounter{corcounter}
\newenvironment{corollary}{%
	\refstepcounter{corcounter}\par\addvspace{3mm}\noindent%
	{\bf Corollary \thecorcounter.\ }%
}{%
	\par\addvspace{3mm}%
}

% bibliography
\renewcommand{\@biblabel}[1]{#1.} % bibliography numbering

\newcommand{\R}{\mathbb{R}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\Z}{\mathbb{Z}}
\newcommand{\CP}{\mathbb{C}}
\newcommand\Paragraph{\textsection}

\newcommand{\keywords}[1]{%
	{\footnotesize\it Keywords:~#1.}%
}

\renewenvironment{abstract}{%
	\footnotesize%
	\begin{adjustwidth}{22mm}{0em}%
	\setlength{\parindent}{2em}%
	\hspace{2em}%
}{%
	\end{adjustwidth}
}

\renewcommand\newblock{\hskip .11em plus .33em minus .07em}
\sloppy
\sfcode`\.=1000\relax

% above and below skips around floats
\setlength{\intextsep}{1em}

% --- captions ---

% subfigure captions
\usepackage{subcaption}
\renewcommand\thesubfigure{\asbuk{subfigure}}
\DeclareCaptionLabelFormat{rightparen}{#2)}
\captionsetup[subfloat]{labelformat=rightparen}

\abovecaptionskip=0pt
\belowcaptionskip=0pt
\newlength{\@tblwid}
\setlength{\@tblwid}{10cm}
\renewcommand{\@makecaption}[2]{%
	\def\@tabenvname{table}%
	\ifx\@currenvir\@tabenvname%
		% we're in a table environment
		\vspace{\abovecaptionskip}%
		\centering\parbox{\@tblwid}{%
			{\flushleft\textbf{#1.} #2\par\vspace{2mm}}%
		}
		\vspace{\belowcaptionskip}%
	\else%
		% we're in a figure environment
		\vspace{\abovecaptionskip}%
		\sbox{\@tempboxa}{{#1. #2}}%
		\ifdim \wd\@tempboxa >\hsize%
			{\textbf{\small #1.} #2}%
		\else%
			\global\@minipagefalse%
			\hbox to \hsize {\textbf{\hfil #1.} #2 \hfil}%
		\fi%
		\vspace{\belowcaptionskip}%
	\fi%
}

% --- table ---
\newcommand{\tabref}[1]{tab.~\ref{#1}} % quick table reference
\newsavebox{\@tblbox}
\newcommand{\tab}[3]{% label, caption, data
	\begin{table}[h]%
	\sbox{\@tblbox}{#3}% put the table to a box
	\settowidth{\@tblwid}{\usebox{\@tblbox}}% measure box width
	%\setlength{\@tblwid}{\linewidth}% measure box width
	\caption{#2}%
	\label{#1}%
	\centering%
	\usebox{\@tblbox}% output the box
	\end{table}%
}

% --- figure ---
\newcommand{\figref}[1]{fig.~\ref{#1}} % quick figure reference
\newcommand{\fig}[3]{% params, label, caption
	\begin{figure}[h]%
		\centering%
		\includegraphics[#1]{#2}%
		\caption{#3}%
		\label{#2}%
	\end{figure}%
}

% --- source code ---
\newcommand{\code}[4][\linewidth]{% width, params, label, caption
	\begin{figure}[h]%
		\centering%
		\topsep=0pt%
		\partopsep=0pt%
		\begin{minipage}{#1}
		{
		\small\VerbatimInput[#2]{#3}

		}%
		\end{minipage}
		\vskip 3pt
		\caption{#4}%
		\label{#3}%
	\end{figure}%
}

% --- unnumbered source code ---
\newcommand{\codenonum}[3]{% params, label, caption
	\begin{figure}[h]%
		\centering%
		\topsep=0pt%
		\partopsep=0pt%
		\begin{minipage}{\linewidth}
		{
		\small\VerbatimInput[#1]{#2}

		}%
		\end{minipage}
		\vskip 3pt
		#3%
		\label{#2}%
	\end{figure}%
}

% --- non-floating figure ---
\newcommand{\fignofloat}[3]{% params, label, caption
	\center%
	\includegraphics[#1]{#2}%
	\caption{#3}%
	\label{#2}%
}

\newcommand{\MakeUppercaseWithNewline}[1]{%
	\begingroup
		\let\SavedOrgNewline\\%
		\DeclareRobustCommand{\\}{\SavedOrgNewline}%
		\MakeTextUppercase{#1}%
	\endgroup
}

\renewcommand\@makefntext[1]{%
	\noindent\@makefnmark #1%
}

% --- acknowledgements ---
\newcommand\ack[1]{
	\vspace{1em}
	{\it
		#1

	}
}

% --- the date the paper was received by the editors ---
\newcommand\received[1]{
	\begin{flushright}
	{\it Received #1.}
	\end{flushright}
}

\newcommand\footnoteref[1]{\protected@xdef\@thefnmark{\ref{#1}}\@footnotemark}

\newcommand\openaccess{%
	\ack{This paper is distributed under the terms of the Creative Commons
	Attribution-Non Commercial 3.0 License which permits non-commercial
	use, reproduction and distribution of the work without further
	permission provided the original work is properly cited.}%
}

% vim: syntax=tex
