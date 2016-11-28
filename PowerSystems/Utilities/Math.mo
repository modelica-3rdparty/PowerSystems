within PowerSystems.Utilities;
package Math "Mathematical functions"
    extends Modelica.Icons.Package;

    function atanVarCut "arc-tangens with variable cut"
      extends Modelica.Icons.Function;

      input Real[2] x "2-dimensional vector";
      input SI.Angle alpha "angle";
      output SI.Angle phi
      "arc(x) with range (alpha-pi) < phi <= (alpha+pi)";
  protected
      Real c;
      Real s;
      import Modelica.Math.atan2;

    algorithm
      c := cos(-alpha);
      s := sin(-alpha);
      phi := atan2({s, c}*x, {c, -s}*x) + alpha;
    annotation (smoothOrder=2,
    Documentation(info="<html>
<p>Genralised atan2 with range
<pre>  (alpha - pi) &lt;  phi &lt; = (alpha + pi)</pre>
i. e. cut at angle <pre>  alpha + pi</pre>
for arbitrary (time-dependent) input argument alpha.</p>
</html>"));
    end atanVarCut;

    function angVelocity "Angular velocity of 2dim vector"
      extends Modelica.Icons.Function;

      input Real[2] x "2-dimensional vector";
      input Real[2] x_dot "time-derivative of x";
      output Real omega "angular velocity of x";
      import Modelica.Constants.eps;

    algorithm
      omega :=(x[1]*x_dot[2] - x_dot[1]*x[2])/(x*x + eps);
    annotation (smoothOrder=2,
    Documentation(info="<html>
<p>Angular velocity omega of 2-dimensional vector x from
<pre>  x and x_dot = der(x)</pre><p>
<p>omega is determined by
<pre>
  omega = datan(x[2]/x[1])*der(x[2]/x[1])
  datan(y) = d atan(y)/dy
</pre><p>
</html>"));
    end angVelocity;

    function mod2sign "Modulo-two sign"
      extends Modelica.Icons.Function;

      input Integer[:] n "integer vector";
      output Integer[size(n, 1)] sign_n "(-1)^n_k, k=1:size(n)";

    algorithm
      for k in 1:size(n, 1) loop
        if n[k] == 2*integer(n[k]/2) then
          sign_n[k] := 1;
      else
          sign_n[k] := -1;
        end if;
      end for;
    annotation(Documentation(info="<html>
<p>Calculates the modulo_2 sign of the integer input vector n, with the following definition:
<pre>
    sign[k] = +1 if n[k] is even
    sign[k] = -1 if n[k] is odd
</pre></p>
</html>"));
    end mod2sign;

    function interpolateTable
    "Interpolation of tables with equidistant arguments"
      extends Modelica.Icons.Function;

      input Real x "table argument";
      input Real[:, :] xy_tab "table, [argument, values]";
      output Real[size(xy_tab, 2) - 1] y "interpolated table values";
  protected
      Integer N1=size(xy_tab, 1);
      Integer N2=size(xy_tab, 2);
      Real x0=xy_tab[1, 1];
      Real del_x=xy_tab[2, 1] - xy_tab[1, 1];
      Real x_rel;
      Integer nx;
      Integer n;

    algorithm
      x_rel := (x - x0)/del_x;
      nx := max(min(integer(x_rel), N1 - 2), 0);
      n := nx + 1;
      y := xy_tab[n, 2:N2] + (x_rel - nx)*(xy_tab[n + 1, 2:N2] - xy_tab[n, 2:N2]);
    annotation (Documentation(info="<html>
<p>Interpolation of tables with one <b>equidistant</b> argument.<br>
The table contains the argument-vector as first column xy_tab[1,:].</p>
<p><tt>y(x)</tt> for <tt>x</tt>-values exceeding the table-range are linearly extrapolated.</p>
</html>"));
    end interpolateTable;

    function polyCoefReal "Coefficients of a polynomial from real roots"
      extends Modelica.Icons.Function;

      input Real[:] r "root vector";
      output Real[size(r,1)+1] c "coefficient vector";
  protected
      parameter Integer n=size(r,1);

    algorithm
      c := cat(1, zeros(n), {1});
      for k in n:-1:1 loop
      c[n:-1:k] := c[n:-1:k] - r[k]*c[n+1:-1:k+1];
      end for;
    annotation (Documentation(info="<html>
<p>The function determines the coefficients <tt>c</tt> of a polynomial of degree n from its <b>real</b> root vector <tt>r</tt>.</p>
<pre>  c_0 + c_1*x + c_2*x^2 + ... + c_n*x^n.</pre>
<p>The resulting <tt>n+1</tt> coefficients are <tt>c[k], k=1 .. n+1</tt>, normalised such that the highest coefficient is one.</p>
<pre>  c[n+1] = 1</pre>
<p><h4>Example</h4></p>
<pre><blockquote>
  Real[3] r={1,2,3};
  Real[4] c;
<b>algorithm</b>
  c := PowerSystems.pu.Functions.polyCoefReal(r);
</blockquote></pre>
<p>The resulting n+1 = 4 coefficients are:</p>
<pre><blockquote>
  c = {-6, 11, -6, 1};
</blockquote><pre>
<p>See also
<a href=\"modelica://PowerSystems.Utilities.Math.polyCoef\">polyCoef</a>, <a href=\"modelica://PowerSystems.Utilities.Math.polyRoots\">polyRoots</a></p>
</html>
"));
    end polyCoefReal;

    function polyCoef "Coefficients of a polynomial from roots"
      extends Modelica.Icons.Function;

      input Real[:,2] r "root vector, 2nd index=1:2, real and imaginary part";
      output Real[size(r,1)+1,2] c
      "coefficient vector, 2nd index=1:2, real and imaginary part";
  protected
      parameter Integer n=size(r,1);

    algorithm
      c := zeros(n+1,2);
      c[n+1,1] := 1;
      for k in n:-1:1 loop
      c[n:-1:k,:] := c[n:-1:k,:] - cat(2, r[k,1]*c[n+1:-1:k+1,1:1] - r[k,2]*c[n+1:-1:k+1,2:2], r[k,1]*c[n+1:-1:k+1,2:2] + r[k,2]*c[n+1:-1:k+1,1:1]);
      end for;
    annotation (Documentation(info="<html>
<p>The function determines the coefficients c of a polynomial of degree n from its root vector r.</p>
<pre>  c_0 + c_1*x + c_2*x^2 + ... + c_n*x^n.</pre>
<p>The resulting <tt>n+1</tt> coefficients are <tt>c[k, :], k=1 .. n+1</tt>, normalised such that the highest coefficient is one.</p>
<pre>
  c[n+1, :] = {1, 0}
  c[k, 1]: real part
  c[k, 2]: imaginary part
</pre>
<p><h4>Example</h4></p>
<pre><blockquote>
  Real[3,2] r=[1,0;2,0;3,0];
  Real[4,2] c;
<b>algorithm</b>
  c := PowerSystems.pu.Functions.polyCoef(r);
</blockquote></pre>
<p>The resulting n+1 = 4 coefficients are:</p>
<pre><blockquote>
  c = [-6, 0; 11, 0; -6, 0; 1, 0];
</blockquote></pre>
<p>See also
<a href=\"modelica://PowerSystems.Utilities.Math.polyCoefReal\">polyCoefReal</a>, <a href=\"modelica://PowerSystems.Utilities.Math.polyRoots\">polyRoots</a></p>
</html>
"));
    end polyCoef;

    function polyRoots "Roots of a polynomial"
      extends Modelica.Icons.Function;

      input Real[:] c "coefficient vector";
      output Real[size(c,1)-1,2] r
      "root vector, 2nd index=1:2, real and imaginary part";
      output Integer N0
      "true deg of polynomial = number of valid roots (r[1:N0,:])";
  protected
      parameter Integer N=size(c,1)-1 "formal degree of polynome";
      Integer n;
      Integer n0;
      Real[N, N] A;
      Real[N+1] C;
      import Modelica.Math.Matrices.eigenValues;

    algorithm
      N0 := N "determine true degree of polymomial";
    //  while c[N0+1] == 0 and N0 > 0 loop
      while abs(c[N0+1])/max(abs(c)) < Modelica.Constants.eps and N0 > 0 loop
        N0 := N0 - 1;
      end while;
      if N0 == 0 then
        r := zeros(N,2);
    else
        n0 := 0;
        while c[n0+1] == 0 loop
          n0 := n0 + 1;
        end while;
        n := N0-n0;
        for k in 1:n+1 loop
          C[k] := c[n0+k];
        end for;
        A[1, 1:n] := -C[n:-1:1]/C[n+1];
        A[2:n,1:n-1] := diagonal(ones(n-1));
        A[2:n,n] := zeros(n-1);
        r[1:n0,:] := zeros(n0,2);
        r[n0+1:n0+n,:] := eigenValues(A[1:n, 1:n]);
      end if;
    annotation (Documentation(info="<html>
<p>The function determines the root vector r of a polynomial of degree N with coefficient vector c.</p>
<pre>  c_0 + c_1*x + c_2*x^2 + ... + c_N*x^N</pre>
<p>The resulting n roots are <tt>r[k, 1:2], k=1 .. n</tt>.</p>
<pre>
  r[k, 1]: real part of kth root
  r[k, 2]: imaginary part of kth root
</pre>
<p>If <tt>c_N</tt> is different from <tt>0</tt> then <tt>n=N</tt>, otherwise <tt>n&lt; N</tt>.</p>
<p><h4>Example</h4></p>
<pre><blockquote>
  Real[3] c = {1,2,3};
  Real[2,2] r;
<b>algorithm</b>
  (r, n) := PowerSystems.pu.Functions.roots(c);
</blockquote></pre>
<p>The resulting n = 2 roots are:</p>
<pre><blockquote>
  r[1,:] = {-0.333333 +0.471405};
  r[2,:] = {-0.333333 -0.471405};
</blockquote></pre>
<p>See also
<a href=\"modelica://PowerSystems.Utilities.Math.polyCoefReal\">polyCoefReal</a>, <a href=\"modelica://PowerSystems.Utilities.Math.polyCoef\">polyCoef</a>, <a href=\"Modelica:Modelica.Math.Matrices.eigenValues\">eigenValues</a></p>
</html>
"));
    end polyRoots;

    function fminSearch
    "Determines minimum of a scalar function with vector-argument x"
      extends Modelica.Icons.Function;

      input Real[:] x0 "start value, fcn(x0) is approximate min";
      input Real[:] x_opt "optional further arguments of function fcn";
      output Real[size(x0,1)] x "argument where function value is minimal";
      output Real y "value of function at x";
  protected
      replaceable function fcn = PowerSystems.Utilities.Precalculation.i_field
      "function to be minimised around x0";
      Integer n = size(x,1);
      Integer max_fun = 200*n;
      Integer max_iter = 200*n;
      constant Real tol_x = 1e-4;
      constant Real tol_f = 1e-4;
      constant Real rho = 1;
      constant Real chi = 2;
      constant Real psi = 0.5;
      constant Real sigma = 0.5;
      constant Real delta=0.05; //for non-zero terms
      constant Real delta0=0.00025; //for zero elements of x
      Integer funcount;
      Integer itercount;
      Integer ifv[n+1];
      Real[n,n+1] v;
      Real fv[n+1];
      Real[n] x1;
      Real[n] xbar;
      Real[n] xr;
      Real fxr;
      Real[n] xe;
      Real fxe;
      Real[n] xc;
      Real fxc;
      Real[n] xcc;
      Real fxcc;
      Boolean shrink;
      String msg;

    algorithm
    // Set up a simplex near the initial guess.
      v := zeros(n,n+1);
      fv := zeros(n+1);
      v[:,1] := x0; //Place input guess in the simplex
      fv[1] := fcn(x0, x_opt);
      funcount := 1;
      itercount := 0;

    // Following improvement suggested by L.Pfeffer at Stanford
      for j in 1:n loop
        x1 := x0;
        x1[j] := if abs(x1[j]) > Modelica.Constants.small then (1 + delta)*x1[j] else delta0;
        v[:,j+1] := x1;
        fv[j+1] := fcn(x1, x_opt);
      end for;

    // Sort so v(1,:) has the lowest function value
      (fv, ifv) := sortUp(fv);
      v := v[:, ifv];
      itercount := itercount + 1;
      funcount := n + 1;

    // Main algorithm
      while (funcount < max_fun and itercount < max_iter) and
        (max(abs(fv[ones(n)] - fv[2:n+1])) > tol_f or
         max(abs(v[:,ones(n)] - v[:,2:n+1])) > tol_x) loop

    // Compute the reflection point
        xbar := sum(v[:,k] for k in 1:n)/n; //average of the n (NOT n+1) best points
        xr := (1 + rho)*xbar - rho*v[:,end];
        fxr := fcn(xr, x_opt);
        funcount := funcount+1;

        if fxr < fv[1] then // Calculate the expansion point
          xe := (1 + rho*chi)*xbar - rho*chi*v[:,end];
          fxe := fcn(xe, x_opt);
          funcount := funcount + 1;
          if fxe < fxr then
            v[:,end] := xe;
            fv[end] := fxe;
        else  v[:,end] := xr;
              fv[end] := fxr;
          end if;
      else
          if fxr < fv[n] then
              v[:,end] := xr;
              fv[end] := fxr;
        else //perform contraction
            if fxr < fv[end] then //outside contraction
              xc := (1 + psi*rho)*xbar - psi*rho*v[:,end];
              fxc := fcn(xc, x_opt);
              funcount := funcount + 1;
              if fxc <= fxr then
                v[:,end] := xc;
                fv[end] := fxc;
                shrink := false;
            else //perform a shrink
                shrink := true;
              end if;
          else //inside contraction
              xcc := (1-psi)*xbar + psi*v[:,end];
              fxcc := fcn(xcc, x_opt);
              funcount := funcount + 1;
              if fxcc < fv[end] then
                v[:,end] := xcc;
                fv[end] := fxcc;
                shrink := false;
            else //perform a shrink
                shrink := true;
              end if;
            end if;
            if shrink then
              for j in 2:n+1 loop
                v[:,j] := v[:,1] + sigma*(v[:,j] - v[:,1]);
                fv[j] := fcn(v[:,j], x_opt);
              end for;
            funcount := funcount + n;
            end if;
          end if;
        end if;

        (fv, ifv) := sortUp(fv);
        v := v[:, ifv];
        itercount := itercount + 1;
      end while;

      x := v[:,1];
    //if isPresent(y) then
      y := fv[1];
    //end if;

      if funcount >= max_fun then
        msg := "fminSearch: max number of function evaluations EXCEEDED";
    elseif itercount >= max_iter then
        msg := "fminSearch: max number of iterations EXCEEDED";
    else
        msg := "fminSearch: terminated successfully";
      end if;
    annotation (Documentation(info="<html>
<p>see Matlab 'fminsearch':<br>
<pre>
 FMINSEARCH Multidimensional unconstrained nonlinear minimization (Nelder-Mead).
    X = FMINSEARCH(FUN,X0) starts at X0 and attempts to find a local minimizer
    X of the function FUN. FUN accepts input X and returns a scalar function
    value F evaluated at X. X0 can be a scalar, vector or matrix.
</pre></p>
<p>Actually only used for precalculation of generator data (fixed function-argument),<br>
Should be modified (domains with boundaries).</p>
</html>"));
    end fminSearch;

    function sortUp "Sorts components of x in increasing order"
      extends Modelica.Icons.Function;

      input Real[:] x "x unsorted";
      output Real[size(x,1)] y "x sorted increasing";
      output Integer[size(x,1)] i "indizes of sorted x";
  protected
      Integer n = size(x,1);
      Integer itemp;
      Real ytemp;

    algorithm
      y := x;
      i := 1:n;
      for j in 1:n-1 loop
        for k in j+1:n loop
          if y[j] > y[k] then
            ytemp := y[j];
            y[j] := y[k];
            y[k] := ytemp;
            itemp := i[j];
            i[j] := i[k];
            i[k] := itemp;
          end if;
        end for;
      end for;
    annotation (Documentation(info="<html>
</html>"));
    end sortUp;

    function sortDown "Sorts components of x in decreasing order"
      extends Modelica.Icons.Function;

      input Real[:] x "x unsorted";
      output Real[size(x,1)] y "x sorted decreasing";
      output Integer[size(x,1)] i "indizes of sorted x";
  protected
      Integer n = size(x,1);
      Integer itemp;
      Real ytemp;

    algorithm
      y := x;
      i := 1:n;
      for j in 1:n-1 loop
        for k in j+1:n loop
          if y[j] < y[k] then
            ytemp := y[j];
            y[j] := y[k];
            y[k] := ytemp;
            itemp := i[j];
            i[j] := i[k];
            i[k] := itemp;
          end if;
        end for;
      end for;
    annotation (Documentation(info="<html>
</html>"));
    end sortDown;

    function relaxation "Exponential relaxation function"
      extends Modelica.Icons.Function;

      input Real t "relative time";
      input Real t_char "characteristic time";
      input Real beta(min=2) "power of exponent";
      output Real[2] y "relaxation function {decreasing, increasing}";
  protected
      final parameter Real gamma=exp(-0.5);
      Real dt=1-t/t_char;

    algorithm
      y[2] := if t < 0 then 1 else if t < t_char then (exp(-0.5*dt^beta) - gamma)/(1 - gamma) else 1;
      y[1] := 1 - y[2];
    annotation (smoothOrder=0,
    Documentation(info="<html>
<p>The function has two components, y[1] decreasing and y[2] increasing.</p>
<p>For
<pre>  0 &le;  t &lt;  t_relax
  y[1] decreases exponentially from 1 to 0
  y[2] increases exponentially from 0 to 1
</pre>
For
<pre>  t &lt;  0 and t &ge  t_relax
  y[1] = 0
  y[2] = 1
</pre>
i.e. for negative t y takes its asymptotic values.</p>
</html>"));
    end relaxation;

    function taylor "Taylor series"
      extends Modelica.Icons.Function;

      input Real x "argument";
      input Real[:] c "coefficients";
      output Real y "sum(c[n]*x^n)";
  protected
      Real x_k;

    algorithm
      y :=1;
      x_k := 1;
      for k in 1:size(c, 1) loop
        x_k := x*x_k;
        y := y + c[k]*x_k;
      end for;
    annotation(Documentation(info="<html>
<p>Calculates the Taylor series
<pre>  y = 1 + sum(c[k]*x^k)</pre></p>
</html>"));
    end taylor;

    function sign_gtlt "Characteristic function abs(x)>b"
      extends Modelica.Icons.Function;

      input Real[:] x "argument";
      input Real b(min=0) "threshold value";
      output Real[size(x,1)] y "characteristic function of abs(x) > b";

    algorithm
      for k in 1:size(x,1) loop
        y[k] := if x[k] > b then 1 else if x[k] < -b then -1 else 0;
      end for;
    annotation(Documentation(info="<html>
<p>Calculates the \"sign\" function
<pre>
  sig = +1 if x[k] &gt;  +b else 0,
  sig = -1 if x[k] &lt;  -b else 0,
</pre>component-wise.</p>
</html>"));
    end sign_gtlt;

    function sign_gt "Sign function x>b"
      extends Modelica.Icons.Function;

      input Real[:] x "argument";
      input Real b(min=0) "threshold value";
      output Real[size(x,1)] y "characteristic function of x > b";

    algorithm
      for k in 1:size(x,1) loop
        y[k] := if x[k] > b then 1 else 0;
      end for;
    annotation(Documentation(info="<html>
<p>Calculates the \"sign\" function
<pre>  sig = 1 if x[k] &gt;  b else 0</pre>component-wise.</p>
</html>"));
    end sign_gt;

    function sign_lt "Sign function x<b"
      extends Modelica.Icons.Function;

      input Real[:] x "argument";
      input Real b(min=0) "threshold value";
      output Real[size(x,1)] y "characteristic function of x < b";

    algorithm
      for k in 1:size(x,1) loop
        y[k] := if x[k] < b then -1 else 0;
      end for;
    annotation(Documentation(info="<html>
<p>Calculates the \"sign\" function
<pre>  sig = -1 if x[k] &lt;  b else 0</pre>component-wise.</p>
</html>"));
    end sign_lt;
    annotation (preferredView="info",
  Documentation(info="<html>
</html>
"));
end Math;

