\usepackage[T1]{fontenc}%
  \usepackage[utf8]{inputenc}%
  \usepackage{amsmath}%
  \usepackage{amsfonts}%
  \usepackage{mathrsfs}%
  \usepackage{amssymb}%
  \usepackage{amsthm}%
  \usepackage[strict]{changepage}%
  \usepackage{graphicx}%
  \usepackage[colorlinks=true,linkcolor=blue,ocgcolorlinks]{hyperref}%
  %\usepackage{cancel}%
  \usepackage[usenames,dvipsnames]{color}%
  %\usepackage{verbatim}%
  %\usepackage{paralist}%
  %\usepackage{xparse}%
  %\usepackage{xstring} %
  \usepackage{environ}%
  \usepackage{framed}%
  \usepackage{mleftright}%
  \usepackage{appendix}%
  \usepackage{mdframed} %
  \usepackage{wrapfig} %
  \usepackage{tabularx} %
  \usepackage[font=small]{caption} %
  \usepackage[all]{xy} %
  % \usepackage{placeins} %
  \usepackage[sectionbib]{chapterbib} %
  \usepackage{xspace}                 %
  \usepackage[ruled,vlined]{algorithm2e}

\renewcommand*{\thefootnote}{\fnsymbol{footnote}}

% et al
\newcommand{\etal}{\textit{et al.}\xspace}

\hypersetup{%
  citecolor = Maroon%
}


\mdfsetup{ %
  innerleftmargin = 1ex, %
  innertopmargin = 1ex, %
  innerrightmargin = 1ex, %
  innerbottommargin = 1ex, %
  skipabove=0pt, %
  skipbelow=0pt, %
  % bottomline=false, %
  % rightline=false, %
  % topline=false, %
}

%%%% No vertical padding for wrapfigure %%%%
  \setlength{\intextsep}{1ex}     %
  \setlength{\columnsep}{1ex}    %

%%%% No extra padding above above or below caption
\setlength{\abovecaptionskip}{.5em} %
  \setlength{\belowcaptionskip}{0pt} %

\newcommand{\extreals}{\reals \cup \setof{-\infty,+\infty}} %
  \newcommand{\pextreals}{\reals \cup \setof{+\infty}} %
  \newcommand{\dualof}{\bar} %
  \newcommand{\argmaxof}[2][]{\argmax_{#1}\setof{#2}} %
  \newcommand{\dnormof}[1]{\normof{#1}_{\star}} %
  \newcommand{\subdiff}{\partial} %
  \newcommand{\ipof}[2]{\seqof{#1,#2}} %

%% math
  \newcommand{\prob}[2]{\text{P}_{#1}\paran{#2}}
\newcommand{\sign}[1]{\text{sign}\left( #1 \right)}
\newcommand{\expec}[2]{\mathbb{E}_{#1}\brac{#2}}
\newcommand{\var}[2]{\mathbb{V}_{#1}\brac{#2}}
\newcommand{\Error}[2]{\text{Err}_{#1}(#2)}
\newcommand{\err}{\text{Err}_{\D}(h)}
\newcommand{\errOpt}{\text{Err}_{\D}(h^*)}
\newcommand{\bra}[1]{ \left\lbrace #1 \right\rbrace }
\newcommand{\brac}[1]{\left[ #1 \right]}
\newcommand{\abs}[1]{\left| #1 \right|}
\newcommand{\paran}[1]{\left( #1 \right)}
\newcommand{\ang}[1]{\left\langle #1 \right\rangle}
\newcommand{\norm}[1]{\left\|#1 \right\|}
\newcommand{\normOne}{\norm{\mathbf{1}}_d}
\newcommand{\expo}{e^{ -\Omega(\gamma) }}
\newcommand{\set}[1]{\left\lbrace #1 \right\rbrace}
\newcommand{\tr}{\mathrm{tr}}
\newcommand{\sconv}{\sigma} %

%% Complexity
  \newcommand{\Th}[1]{\Theta \paran{#1}}
\newcommand{\Oh}[1]{O \paran{#1}}
\newcommand{\oh}[1]{o \paran{#1}}
\newcommand{\Om}[1]{\Omega \paran{#1}}
\newcommand{\poly}[1]{\text{poly}\paran{#1}}


%%%%%%%%%%%%%%%% MATH COMMANDS %%%%%%%%%%%%%%%%


\newcommand{\fix}{\marginpar{FIX}} %
  \newcommand{\new}{\marginpar{NEW}} %
  %% indicator function
\newcommand{\indicate}[1]{I_{\bracketsof{#1}}}
\newcommand{\delivered}{\mathcal{F}}
\newcommand{\deliveredat}[1]{\delivered_{#1}}
\newcommand{\totaldelay}{D}
\newcommand{\delayedleader}{\leader^{\operatorname{d}}} %
  \newcommand{\drunkdelayedleader}{\drunkleader^{\operatorname{d}}} %


%%%%%%%% Calculus %%%%%%%%

%%%% Gradients %%%%
\newcommand{\gradient}{\nabla} %
  \newcommand{\gradientat}[2]{\gradient \! #2 |_{#1}} %


%%%%%%%% Sub-Gaussians and Sub-Exponentials %%%%%%%%

% Sub-Gaussian Norm
  \newcommand{\sgnorm}[1]{\norm{#1}_{\psi_2}} %
  \newcommand{\sgnormof}{\sgnorm}             %

% Sub-Exponential Norm
  \newcommand{\senorm}[1]{\norm{#1}_{\psi_1}} %
  \newcommand{\senormof}{\senorm} %

%%%%%%%% GEOMETRY %%%%%%%%

%%%% Balls %%%%
\newcommand{\unitball}{B_1}     %
  \newcommand{\ball}{B}           %

%%%% Volume %%%%
\newcommand{\volume}{\operatorname{vol}} %
  \newcommand{\volumeof}{\volume\parof}    %

%%%%%%%% NUMBERS %%%%%%%%%

\newcommand{\reals}{\mathbb{R}} %
  \newcommand{\preals}{\reals_{>0}} %
  \newcommand{\nnreals}{\reals_{\geq 0}} %
  \newcommand{\integers}{\mathbb{Z}} %
  \newcommand{\nnintegers}{\integers^{+}}
\newcommand{\naturalnumbers}{\mathbb{N}} %


%%%%%%%% GROUPINGS %%%%%%%%

%%%% Size-of
\newcommand{\sizeof}[1]{\mleft| #1 \mright|}    %
  %%%% Set-of
\newcommand{\setof}[1]{\mleft \{ #1 \mright\}}  %
  %%%% Parenthesis-of
\newcommand{\parof}[1]{\mleft( #1 \mright)} %
  %%%% Brackets-of
\newcommand{\bracketsof}[1]{\mleft[#1 \mright]} %
  %%%% Norm
  \newcommand{\loneof}[1]{\norm{#1}_1} %
  \newcommand{\lonenormof}{\loneof} %
  \newcommand{\taxinormof}{\lonenormof} %
  \newcommand{\maxnormof}[1]{\normof{#1}_{\infty}} %
  \newcommand{\normof}{\norm} %
  \newcommand{\eunormof}[1]{\normof{#1}_2} %
  %%%% Absolute value of
  \newcommand{\absvof}{\sizeof}
%%%% Paranthesis around fraction
  \newcommand{\prac}[2]{\parof{\frac{#1}{#2}}}
%%%% Sequence-of
\newcommand{\seqof}[1]{\left \langle #1 \right \rangle}

%%%% Exponent of
\newcommand{\expof}{\exp\parof}
%%%% Natural algorithm of
  \newcommand{\lnof}[1][]{\ln_{#1}\parof}
%%%% Log of
\newcommand{\logof}[1][]{\log_{#1}\parof}
%%%% Polylog
  \newcommand{\polylog}{\operatorname{polylog}} %
  \newcommand{\polylogof}{\polylog\parof} %


%%%% Equal by Definition
  \newcommand{\defeq}{\overset{\operatorname{def}}{=}}
%%%%%%%% ROUNDING %%%%%%%%

%%%% round-up
\newcommand{\roundup}[1]{\mleft\lceil #1 \mright\rceil}     %
  %%%% round-down
\newcommand{\rounddown}[2][]{\mleft\lfloor #2 \mright\rfloor_{#1}} %
  %%%% round fraction up
  \newcommand{\fracup}[2]{\roundup{\frac{#1}{#2}}}            %
  %%%% round fraction down
  \newcommand{\fracdown}[2]{\rounddown{\frac{#1}{#2}}}        %
  %%%% half
  \newcommand{\half}[1]{\frac{#1}{2}}                         %
  %%%% half, rounded down
  \newcommand{\halfdown}[1]{\fracdown{#1}{2}}                 %

%%%%%%%% SETS %%%%%%%%

\newcommand{\setA}{A}
\newcommand{\setB}{B}
\newcommand{\setC}{C}

%% Symmetric difference
\newcommand{\symd}{\Delta}


%% Subsets
  \newcommand{\subsetsof}[1]{2^{#1}}
\newcommand{\powerset}[1]{2^{#1}}
\newcommand{\oddsubsetsof}[1]{\subsetsof{#1}_{\operatorname{odd}}}

%%%%%%%% OPTIMIZATION %%%%%%%%

\newcommand{\opt}{\mathrm{OPT}}
\newcommand{\OPT}{\mathrm{OPT}}

%%%% Weights
  \newcommand{\weight}{w}
%%%% Costs
  \newcommand{\cost}{c}
\newcommand{\costFunc}{f}


%%%%%%%% PROBABILITY THEORY %%%%%%%%

\newcommand{\expectedvalue}{\mathbf{E}}%
  \renewcommand{\Pr}{\mathbf{P}}%
  \newcommand{\probof}{\Pr\bracketsof}%
  \newcommand{\evof}[1][]{\expectedvalue_{#1}\bracketsof}%

\newcommand{\events}{\mathcal{X}} %
  \newcommand{\eventsB}{\mathcal{Y}} %
  \newcommand{\event}{X}             %
  \newcommand{\notevent}{\overline{X}} %
  \newcommand{\eventA}[1][]{X_{#1}}    %
  \newcommand{\noteventA}[1][]{\overline{\eventA[#1]}} %
  \newcommand{\eventB}[1][]{Y_{#1}}                    %
  \newcommand{\noteventB}[1][]{\overline{\eventB[#1]}} %



%%%%%%%% Arrows %%%%%%%%
%% Injections %%
\newcommand{\into}{\hookrightarrow}
\newcommand{\linto}{\lhook\joinrel\longrightarrow}
\newcommand{\xinto}[2][]{\underset{#1}{\overset{#2}{\linto}}}
\newcommand{\xto}{\xrightarrow}

%% Surjections %%
\makeatletter
  \providecommand*{\twoheadrightarrowfill@}{%
  \arrowfill@\relbar\relbar\twoheadrightarrow
}
\providecommand*{\xtwoheadrightarrow}[2][]{%
  \ext@arrow 0579\twoheadrightarrowfill@{#1}{#2}%
}
\makeatother
  \newcommand{\onto}{\twoheadrightarrow}
\newcommand{\xonto}{\xtwoheadrightarrow}

%% Bijections and Isomorphisms %%
\newcommand{\bijection}{\longleftrightarrow}%
  \makeatletter
  \newcommand\longleftrightarrowfill@{%
  \arrowfill@\leftarrow\relbar\rightarrow}%
  \makeatother
  \newcommand{\xbijection}{\xleftrightarrow}%
  \newcommand{\isomorphism}[1][]{\xrightarrow[
  \,\smash{\raisebox{0.8ex}{\ensuremath{\scriptstyle\sim}}}\,]{#1}}
\newcommand{\isom}{\isomorphism}%
  \newcommand{\xisom}[1]{\isom[#1]}%

% Where, suchthat
\newcommand{\where}{:}
\newcommand{\suchthat}{\where}
\newcommand{\given}{|}

% Max and Min
  \newcommand{\argmax}{\operatorname*{arg \, max}} %
  \newcommand{\argmin}{\operatorname*{arg \, min}} %
  \newcommand{\argminof}[2][]{\argmin_{#1}\setof{#2}}

% Posets
  \newcommand{\peq}{\preceq} %
  \newcommand{\poset}{\mathcal{P}}

% Game theory
\newcommand{\playerA}{A}%
  \newcommand{\playerB}{B}%
  \newcommand{\playerC}{C}%
  \newcommand{\profit}{\gamma}%
  \newcommand{\profitat}[1]{\profit_{#1}}%
  \newcommand{\alternative}{\alpha}%
  \newcommand{\alternativeat}[1]{\alternative_{#1}}

%%%%%%%% LEARNING THEORY %%%%%%%%

\newcommand{\loss}{\ell} %
  \newcommand{\A}{\mathcal{A}} %
  \newcommand{\C}{\mathcal{C}} %
  \newcommand{\F}{\mathcal{F}} %
  \newcommand{\G}{\mathcal{G}} %
  \newcommand{\Hy}{\mathcal{H}} %
  \newcommand{\K}{\mathcal{K}} %
  \newcommand{\X}{\mathcal{X}} %
  \newcommand{\Y}{\mathcal{Y}} %
  \newcommand{\Z}{\mathcal{Z}} %
  \newcommand{\U}{\mathcal{U}} %
  \newcommand{\Po}{\mathcal{P}}
\newcommand{\Dx}{D_\mathcal{X}} %
  \newcommand{\Dy}{D_\mathcal{Y}} %
  \newcommand{\Dxy}{D_\mathcal{X \times Y}} %
  \newcommand{\ndim}[1]{\text{nDim}(#1)} %
  \newcommand{\samples}{S = \set{(x_i, y_i)}_{i=1}^m \in (\X \times \Y)^m  } %
  \newcommand{\x}{\mathbf{x}} %
  \newcommand{\y}{\mathbf{y}}
\newcommand{\w}{\mathbf{w}}

% polynomial
  \newcommand{\size}[1]{\text{size}\paran{#1}}

%%
\newcommand{\risk}{R(h)} %
  %\newcommand{\empRisk}[2][h, ]{\hat{R}_{#2}(#1)} %
  \DeclareDocumentCommand{\empRisk}{ O{h} O{} }{\hat{R}_{#2}(#1)}


% Rademacher complexities
\newcommand{\rmacher}[3][]{\mathfrak{R}^{#1}_{#2}(#3)}
\newcommand{\empRmacher}[3][]{\hat{\mathfrak{R}}^{#1}_{#2}(#3)}

%% margin-related stuff
  \newcommand{\margin}{\rho_h\paran{x,y}} %
  \newcommand{\marginDef}[1][]{ h(x_{#1},y) - \max_{y' \neq y} h(x_{#1},y') } %
  \newcommand{\marginLoss}{\ell_\rho(\margin)}


%% vc dimension stuff
  \newcommand{\growth}[2]{\Pi_{#1}(#2)}

%%%% Follow-The-Leader %%%%

\newcommand{\leader}{y}                 %
  \newcommand{\lazyleader}{\overline{\leader}} %

\newcommand{\prophet}{z}        %


% Drunk prophet
\newcommand{\drunkprophet}{\tilde{\prophet}} %
  \newcommand{\dprophet}{\drunkprophet}        %

% Drunk leader
\newcommand{\drunkleader}{\tilde{\leader}} %
  \newcommand{\dleader}{\tilde{\leader}}     %

% Bandit

\renewcommand{\loss}{\ell} %
  \newcommand{\lossat}[1]{\loss_{#1}} %
  \newcommand{\lossB}{\loss'} %
  \newcommand{\costfor}[2]{c_{#1,#2}} %
  \newcommand{\rounds}{T} %
  \newcommand{\round}{t} %
  \newcommand{\roundloss}{\lossat{\round}} %
  \newcommand{\experts}{n} %
  \newcommand{\expert}{i} %
  \newcommand{\roundexpertcost}{\costfor{\expert}{\round}}


%% covering number
\newcommand{\covering}[1]{\mathcal{N} (#1)}



%%%%%%%%%%%%%%%% ALGORITHMS %%%%%%%%%%%%%%%%

\newenvironment{pseudocode}{ \tt %
  % \begin{quote}
  \begin{minipage}{\linewidth}
  % \begin{framed}
}{
  % \end{framed}
  \end{minipage}
  % \end{quote}
}


\newenvironment{theroutine}[1]{
  \normalsize
    \vspace{-\partopsep}
  \begin{tabbing}
  \quad\=\quad\=\quad\=\quad\=\quad\=\quad\=\kill
    {\large #1} \+ \\
}{%
  %
  \end{tabbing}
  \vspace{-\baselineskip}
  \vspace{-\partopsep}
}

\DeclareDocumentEnvironment{routine}{o m g}{
  \begin{theroutine}{%
    \IfNoValueTF{#1}{%
      \IfNoValueTF{#3}{\newalgo{#2}}{\newalgo{#2}{#3}}%
    }{%
      \IfNovalueTF{#3}{\newalgo[#1]{#2}}{\newalgo[#1]{#2}{#3}}%
    }%
  }%
}{%
  \end{theroutine}%
}

% Naming algorithms
\newcommand{\algo}[1]{%
{\normalfont%
  \texttt{%
    \StrSubstitute{#1}{-}{-{\allowbreak}}%
  }%
}%
}

% New terms with links
\DeclareDocumentCommand{\newalgo}{o m g}{%
  \IfNoValueTF{#1}{%
    \hypertarget{algo:#2}{%
      \algo{#2\IfNoValueF{#3}{(#3)}}%
    }%
  }{%
    \hypertarget{algo:#1}{%
      \algo{#2\IfNoValueF{#3}{(#3)}}%
    }%
  }%
}


\DeclareDocumentCommand{\refalgo}{o m g}{%
  \IfNoValueTF{#1}{%
    \hyperlink{algo:#2}{\algo{%
      \StrSubstitute[0]{#2}{-}{-{\allowbreak}}%
        \IfNoValueF{#3}{(#3)}}%
    }%
  }{%
    \hyperlink{algo:#1}{\algo{#2\IfNoValueF{#3}{(#3)}}}%
  }%
}

% Code comments
\newcommand{\commentcode}[1]{{\color{ForestGreen} // #1}}


  %%%%%%%%%%%%%%%% FORMATTING COMMANDS %%%%%%%%%%%%%%%%%%


  % Puts the equation on the left hand side of an align* environment.
  \newcommand{\lhs}[1][2]{\hspace{#1em}&\hspace{-#1em}}

  %% Theorems

  \newtheorem{lemma}{Lemma}[section]
  \newtheorem{definition}{Definition}[section]
  \newtheorem{proposition}{Proposition}[section]
  \newtheorem{theorem}[lemma]{Theorem}
  \newtheorem{corollary}[lemma]{Corollary}
  \newtheorem{conjecture}[lemma]{Conjecture}

  \theoremstyle{remark}
  \newtheorem{example}[lemma]{Example}
  \newtheorem{remark}[lemma]{Remark}



  %%%%%%%% LISTS %%%%%%%%

  %% Properties List
  \newenvironment{properties}{
    \begin{compactenum}[(i)]
  }{
    \end{compactenum}
  }


  %% Lemma List
  \newenvironment{lemmalist}{
    \begin{compactenum}[(a)]
  }{
    \end{compactenum}
  }
  % List of results
    \newenvironment{results}{
    \begin{compactenum}[(a)]
  }{
    \end{compactenum}
  }

  %%%% List of definitions
    \newenvironment{definitions}{
    \begin{compactitem}[$-$]
  }{
    \end{compactitem}
  }

  % List of cases


  %% Labels

  %% Figures
    \newcommand{\labelfigure}[1]{\label{figure:#1}}
  \newcommand{\reffigure}[1]{\hyperref[figure:#1]{Figure \ref*{figure:#1}}}

  %% Sections
    \newcommand{\labelsection}[1]{\label{section:#1}}
  \newcommand{\refsection}[1]{\hyperref[section:#1]{Section \ref*{section:#1}}}


  %% Appendixs
    \newcommand{\labelappendix}[1]{\label{appendix:#1}}
  \newcommand{\refappendix}[1]{\hyperref[appendix:#1]{Appendix \ref*{appendix:#1}}}

  %% Chapters
    \newcommand{\labelchapter}[1]{\label{chapter:#1}}
  \newcommand{\refchapter}[1]{\hyperref[chapter:#1]{Chapter
    \ref*{chapter:#1}}}

  %% Lemmas
    \newcommand{\labellemma}[1]{\label{lemma:#1}}
  \newcommand{\reflemma}[1]{\hyperref[lemma:#1]{Lemma \ref*{lemma:#1}}}

  %% Examples
    \newcommand{\labelexample}[1]{\label{example:#1}}
  \newcommand{\refexample}[1]{\hyperref[example:#1]{example
    \ref*{example:#1}}}
  \newcommand{\refexamples}[2]{\hyperref[example:#1]{examples
    \ref*{example:#1} -- \ref*{example:#2}}}

  %% Theorems
    \newcommand{\labeltheorem}[1]{\label{theorem:#1}}
  \newcommand{\reftheorem}[1]{\hyperref[theorem:#1]{Theorem \ref*{theorem:#1}}}

  %% Corollary
    \newcommand{\labelcorollary}[1]{\label{corollary:#1}}
  \newcommand{\refcorollary}[1]{\hyperref[corollary:#1]{Corollary \ref{corollary:#1}}}

  %% Equations
    \newcommand{\labelequation}[1]{\label{equation:#1}}
  \newcommand{\refequation}[1]{equation \hyperref[equation:#1]{(\ref*{equation:#1})}}

  \newcommand{\labelexercise}[1]{\label{exercise:#1}}
  \newcommand{\refexercise}[1]{exercise \ref{exercise:#1}}

  \renewcommand{\qedsymbol}{$\blacksquare$}




  \makeatletter
    \def\blfootnote{\xdef\@thefnmark{}\@footnotetext}
  \makeatother

  \NewEnviron{footer}{\blfootnote{\vspace{-4ex}\BODY\vspace{-4ex}}}


  % Editing notes

  % New terms with links
  \DeclareDocumentCommand{\newterm}{o m}{%
    \IfNoValueTF{#1}{%
      % \label{def:#2}%
        \hypertarget{def:#2}{\textit{#2}}%
    }{%
      % \label{def:#1}%%
        \hypertarget{def:#1}{\textit{#2}}%
    }%
  }

  \DeclareDocumentCommand{\refterm}{o m}{%
    \IfNoValueTF{#1}{%
      \hyperlink{def:#2}{#2}%
    }{%
      \hyperlink{def:#1}{#2}%
    }%
  }

  %%%% Number only this line
    \newcommand\numberthis{\addtocounter{equation}{1}\tag{\theequation}}



  % Editing notes
  \newcommand{\note}[1]{{\large\bfseries\ttfamily\color{red}#1}}
  \newcommand{\daniel}[1]{{\large\bfseries\ttfamily\color{blue}#1}}


  %% to add reference to a footnote
  \makeatletter
    \newcommand\footnoteref[1]{\protected@xdef\@thefnmark{\ref{#1}}\@footnotemark}
  \makeatother
