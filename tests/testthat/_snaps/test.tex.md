# Normal output: mix.type = 0

    Code
      cat(x.tex, sep = "\n")
    Output
      \begin{align}
      g(x) &= f(x),\\
      \Psi(x) &= \Phi(x),\\
      \\
      f(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \exp \left( -\dfrac{(x - \mu)^2}{2 \sigma^2} \right),\\
      \Phi(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu)^2}{2 \sigma^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu = 1, & \sigma = 0.5.
      \end{array}
      \end{align}

# Normal output: mix.type = 1

    Code
      cat(x.tex, sep = "\n")
    Output
      \begin{align}
      g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) ),\\
      \Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) ),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# Normal output: mix.type = 2

    Code
      cat(x.tex, sep = "\n")
    Output
      \begin{align}
      g(x) &= \left( 1 - \Phi_1(x) \right) f_1(x) + \Phi_2(x) f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{2} \Phi_1(x)^2 + \dfrac{1}{2} \Phi_2(x)^2,\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# Normal output: mix.type = 3, v2

    Code
      cat(x.tex, sep = "\n")
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# Normal output: mix.type = 3, v3

    Code
      cat(x.tex, sep = "\n")
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1,\\
      \mu_3 = 3, & \sigma_3 = 1.5.
      \end{array}
      \end{align}

# Normal output: mix.type = 4

    Code
      cat(x.tex, sep = "\n")
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 0.5,\\
      \mu_{1,2} = 2, & \sigma_{1,2} = 1,\\
      \mu_{2,1} = 3, & \sigma_{2,1} = 1.5,\\
      \mu_{2,2} = 4, & \sigma_{2,2} = 2.
      \end{array}
      \end{align}

# tex.d: mix.type = 0

    Code
      cat(x.tex.d, sep = "\n")
    Output
      \begin{align}
      g(x) &= f(x),\\
      \\
      f(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \exp \left( -\dfrac{(x - \mu)^2}{2 \sigma^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu = 1, & \sigma = 0.5.
      \end{array}
      \end{align}

# tex.d: mix.type = 1

    Code
      cat(x.tex.d, sep = "\n")
    Output
      \begin{align}
      g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) ),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# tex.d: mix.type = 2

    Code
      cat(x.tex.d, sep = "\n")
    Output
      \begin{align}
      g(x) &= \left( 1 - \Phi_1(x) \right) f_1(x) + \Phi_2(x) f_2(x),\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# tex.d: mix.type = 3, v2

    Code
      cat(x.tex.d, sep = "\n")
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# tex.d: mix.type = 3, v3

    Code
      cat(x.tex.d, sep = "\n")
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1,\\
      \mu_3 = 3, & \sigma_3 = 1.5.
      \end{array}
      \end{align}

# tex.d: mix.type = 4

    Code
      cat(x.tex.d, sep = "\n")
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 0.5,\\
      \mu_{1,2} = 2, & \sigma_{1,2} = 1,\\
      \mu_{2,1} = 3, & \sigma_{2,1} = 1.5,\\
      \mu_{2,2} = 4, & \sigma_{2,2} = 2.
      \end{array}
      \end{align}

# tex.p: mix.type = 0

    Code
      cat(x.tex.p, sep = "\n")
    Output
      \begin{align}
      \Psi(x) &= \Phi(x),\\
      \\
      \Phi(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu)^2}{2 \sigma^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu = 1, & \sigma = 0.5.
      \end{array}
      \end{align}

# tex.p: mix.type = 1

    Code
      cat(x.tex.p, sep = "\n")
    Output
      \begin{align}
      \Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) ),\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# tex.p: mix.type = 2

    Code
      cat(x.tex.p, sep = "\n")
    Output
      \begin{align}
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{2} \Phi_1(x)^2 + \dfrac{1}{2} \Phi_2(x)^2,\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# tex.p: mix.type = 3, v2

    Code
      cat(x.tex.p, sep = "\n")
    Output
      \begin{align}
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1.
      \end{array}
      \end{align}

# tex.p: mix.type = 3, v3

    Code
      cat(x.tex.p, sep = "\n")
    Output
      \begin{align}
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 0.5,\\
      \mu_2 = 2, & \sigma_2 = 1,\\
      \mu_3 = 3, & \sigma_3 = 1.5.
      \end{array}
      \end{align}

# tex.p: mix.type = 4

    Code
      cat(x.tex.p, sep = "\n")
    Output
      \begin{align}
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 0.5,\\
      \mu_{1,2} = 2, & \sigma_{1,2} = 1,\\
      \mu_{2,1} = 3, & \sigma_{2,1} = 1.5,\\
      \mu_{2,2} = 4, & \sigma_{2,2} = 2.
      \end{array}
      \end{align}

# Changing sep value

    Code
      a$tex(sep = "--")
    Output
      \begin{align}--g(x) &= f(x),\\--\Psi(x) &= \Phi(x),\\--\\--f(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \exp \left( -\dfrac{(x - \mu)^2}{2 \sigma^2} \right),\\--\Phi(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu)^2}{2 \sigma^2} \right) dt,\\--\\--& \begin{array}{ll}--\mu = 1, & \sigma = 0.5.--\end{array}--\end{align}--

---

    Code
      a$tex.d(sep = "+++")
    Output
      \begin{align}+++g(x) &= f(x),\\+++\\+++f(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \exp \left( -\dfrac{(x - \mu)^2}{2 \sigma^2} \right),\\+++\\+++& \begin{array}{ll}+++\mu = 1, & \sigma = 0.5.+++\end{array}+++\end{align}+++

---

    Code
      a$tex.p(sep = "****")
    Output
      \begin{align}****\Psi(x) &= \Phi(x),\\****\\****\Phi(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu)^2}{2 \sigma^2} \right) dt,\\****\\****& \begin{array}{ll}****\mu = 1, & \sigma = 0.5.****\end{array}****\end{align}****

# Changing frac.env value

    Code
      a$tex(frac.env = "array")
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 2, & \sigma_2 = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$tex(frac.env = "aligned")
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{aligned} \dfrac{\sigma_i}{\sqrt{2}} \end{aligned} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{aligned} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{aligned} \dfrac{\sigma_i}{\sqrt{2}} \end{aligned} \right)^2} \end{aligned} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 2, & \sigma_2 = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$tex(frac.env = "gathered")
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{gathered} \dfrac{\sigma_i}{\sqrt{2}} \end{gathered} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{gathered} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{gathered} \dfrac{\sigma_i}{\sqrt{2}} \end{gathered} \right)^2} \end{gathered} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 2, & \sigma_2 = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$tex(frac.env = "default")
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \dfrac{\sigma_i}{\sqrt{2}} \right)^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \left( \dfrac{\sigma_i}{\sqrt{2}} \right)^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 2, & \sigma_2 = 2.5.
      \end{array}
      \end{align}

# Outputs of eq.mean and eq.sd objects: mix.type = 1

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = 1:2 + 0.5), this.mix.type = 1)$tex()
    Output
      \begin{align}
      g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) ),\\
      \Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) ),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = 1:2, sd = c(1.5, 1.5)), this.mix.type = 1)$tex()
    Output
      \begin{align}
      g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) ),\\
      \Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) ),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 2, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), this.mix.type = 1)$tex()
    Output
      \begin{align}
      g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) ),\\
      \Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) ),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), this.mix.type = 1)$
      tex.d()
    Output
      \begin{align}
      g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) ),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), this.mix.type = 1)$
      tex.p()
    Output
      \begin{align}
      \Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) ),\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

# Outputs of eq.mean and eq.sd objects: mix.type = 2

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = 1:2 + 0.5), this.mix.type = 2)$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Phi_1(x) \right) f_1(x) + \Phi_2(x) f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{2} \Phi_1(x)^2 + \dfrac{1}{2} \Phi_2(x)^2,\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = 1:2, sd = c(1.5, 1.5)), this.mix.type = 2)$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Phi_1(x) \right) f_1(x) + \Phi_2(x) f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{2} \Phi_1(x)^2 + \dfrac{1}{2} \Phi_2(x)^2,\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 2, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), this.mix.type = 2)$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Phi_1(x) \right) f_1(x) + \Phi_2(x) f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{2} \Phi_1(x)^2 + \dfrac{1}{2} \Phi_2(x)^2,\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), this.mix.type = 2)$
      tex.d()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Phi_1(x) \right) f_1(x) + \Phi_2(x) f_2(x),\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), this.mix.type = 2)$
      tex.p()
    Output
      \begin{align}
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{2} \Phi_1(x)^2 + \dfrac{1}{2} \Phi_2(x)^2,\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

# Outputs of eq.mean and eq.sd objects: mix.type = 3, v2

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = 1:2 + 0.5), grad = "v2")$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = 1:2, sd = c(1.5, 1.5)), grad = "v2")$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 2, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), grad = "v2")$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), grad = "v2")$tex.d()
    Output
      \begin{align}
      g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1), sd = c(1.5, 1.5)), grad = "v2")$tex.p()
    Output
      \begin{align}
      \Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1.
      \end{array}
      \end{align}

# Outputs of eq.mean and eq.sd objects: mix.type = 3, v3

    Code
      a$set.cmp(data.frame(mean = c(1, 1, 1), sd = 1:3 + 0.5), grad = "v3")$tex()
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = 2.5,\\
      \mu_3 = \mu_1, & \sigma_3 = 3.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = 1:3, sd = c(1.5, 1.5, 1.5)), grad = "v3")$tex()
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 2, & \sigma_2 = \sigma_1,\\
      \mu_3 = 3, & \sigma_3 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1, 1), sd = c(1.5, 1.5, 1.5)), grad = "v3")$
      tex()
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1,\\
      \mu_3 = \mu_1, & \sigma_3 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1, 1), sd = c(1.5, 1.5, 1.5)), grad = "v3")$
        tex.d()
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1,\\
      \mu_3 = \mu_1, & \sigma_3 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1, 1), sd = c(1.5, 1.5, 1.5)), grad = "v3")$
        tex.p()
    Output
      \begin{align}
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = \mu_1, & \sigma_2 = \sigma_1,\\
      \mu_3 = \mu_1, & \sigma_3 = \sigma_1.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1, 2), sd = c(1.5, 1.5, 2.5)), grad = "v3")$
      tex()
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 1, & \sigma_1 = 1.5,\\
      \mu_2 = 1, & \sigma_2 = 1.5,\\
      \mu_3 = 2, & \sigma_3 = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(2, 1, 2), sd = c(2.5, 1.5, 2.5)), grad = "v3")$
      tex()
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 2, & \sigma_1 = 2.5,\\
      \mu_2 = 1, & \sigma_2 = 1.5,\\
      \mu_3 = 2, & \sigma_3 = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(2, 1, 1), sd = c(2.5, 1.5, 1.5)), grad = "v3")$
      tex()
    Output
      \begin{align}
      g(x) &= g_1(x) + g_2(x) + g_3(x) ,\\
      \Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x),\\
      \\
      g_1(x) &= \left\lbrace
      \begin{array}{ll}
      \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1),\\
      0 & (x > \mu_1),\\
      \end{array} \right.\\
      g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x),\\
      g_3(x) &= \left\lbrace \begin{array}{ll}0 & (x < \mu_3),\\
      \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3),
      \end{array} \right.\\
      \Psi_1(x) &= \mathrm{min} \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right),\\
      \Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x),\\
      \Psi_3(x) &= \mathrm{max} \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right),\\
      \\
      f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu_i)^2}{2 \sigma_i^2} \right),\\
      \Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_i)^2}{2 \sigma_i^2} \right) dt,\\
      \Phi^\ast_i(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_i)^2}{2 \left( \begin{array}{c} \dfrac{\sigma_i}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_1 = 2, & \sigma_1 = 2.5,\\
      \mu_2 = 1, & \sigma_2 = 1.5,\\
      \mu_3 = 1, & \sigma_3 = 1.5.
      \end{array}
      \end{align}

# Outputs of eq.mean and eq.sd objects: mix.type = 4 -- 1/2

    Code
      a$set.cmp(data.frame(mean = rep(1, 4), sd = 1:4 + 0.5), this.mix.type = 4)$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = \mu_{1,1}, & \sigma_{1,2} = 2.5,\\
      \mu_{2,1} = \mu_{1,1}, & \sigma_{2,1} = 3.5,\\
      \mu_{2,2} = \mu_{1,1}, & \sigma_{2,2} = 4.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = 1:4, sd = rep(1.5, 4)), this.mix.type = 4)$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = 2, & \sigma_{1,2} = \sigma_{1,1},\\
      \mu_{2,1} = 3, & \sigma_{2,1} = \sigma_{1,1},\\
      \mu_{2,2} = 4, & \sigma_{2,2} = \sigma_{1,1}.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = rep(1, 4), sd = rep(1.5, 4)), this.mix.type = 4)$
      tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = \mu_{1,1}, & \sigma_{1,2} = \sigma_{1,1},\\
      \mu_{2,1} = \mu_{1,1}, & \sigma_{2,1} = \sigma_{1,1},\\
      \mu_{2,2} = \mu_{1,1}, & \sigma_{2,2} = \sigma_{1,1}.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = rep(1, 4), sd = rep(1.5, 4)), this.mix.type = 4)$
        tex.d()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = \mu_{1,1}, & \sigma_{1,2} = \sigma_{1,1},\\
      \mu_{2,1} = \mu_{1,1}, & \sigma_{2,1} = \sigma_{1,1},\\
      \mu_{2,2} = \mu_{1,1}, & \sigma_{2,2} = \sigma_{1,1}.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = rep(1, 4), sd = rep(1.5, 4)), this.mix.type = 4)$
        tex.p()
    Output
      \begin{align}
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = \mu_{1,1}, & \sigma_{1,2} = \sigma_{1,1},\\
      \mu_{2,1} = \mu_{1,1}, & \sigma_{2,1} = \sigma_{1,1},\\
      \mu_{2,2} = \mu_{1,1}, & \sigma_{2,2} = \sigma_{1,1}.
      \end{array}
      \end{align}

# Outputs of eq.mean and eq.sd objects: mix.type = 4 -- 2/2

    Code
      a$set.cmp(data.frame(mean = c(1, 1, 2, 2), sd = c(1.5, 1.5, 2.5, 2.5)),
      this.mix.type = 4)$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = 1, & \sigma_{1,2} = 1.5,\\
      \mu_{2,1} = 2, & \sigma_{2,1} = 2.5,\\
      \mu_{2,2} = 2, & \sigma_{2,2} = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(2, 1, 1, 2), sd = c(2.5, 1.5, 1.5, 2.5)),
      this.mix.type = 4)$tex()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 2, & \sigma_{1,1} = 2.5,\\
      \mu_{1,2} = 1, & \sigma_{1,2} = 1.5,\\
      \mu_{2,1} = 1, & \sigma_{2,1} = 1.5,\\
      \mu_{2,2} = 2, & \sigma_{2,2} = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 2, 1, 2), sd = c(1.5, 2.5, 1.5, 2.5)),
      this.mix.type = 4)$tex.d()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = 2, & \sigma_{1,2} = 2.5,\\
      \mu_{2,1} = 1, & \sigma_{2,1} = 1.5,\\
      \mu_{2,2} = 2, & \sigma_{2,2} = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 2, 2, 2), sd = c(1.5, 2.5, 2.5, 2.5)),
      this.mix.type = 4)$tex.d()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = 2, & \sigma_{1,2} = 2.5,\\
      \mu_{2,1} = 2, & \sigma_{2,1} = 2.5,\\
      \mu_{2,2} = 2, & \sigma_{2,2} = 2.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 2, 1, 1), sd = c(1.5, 2.5, 1.5, 1.5)),
      this.mix.type = 4)$tex.d()
    Output
      \begin{align}
      g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x),\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x),\\
      f_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \exp \left( -\dfrac{(x - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right),\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = 2, & \sigma_{1,2} = 2.5,\\
      \mu_{2,1} = 1, & \sigma_{2,1} = 1.5,\\
      \mu_{2,2} = 1, & \sigma_{2,2} = 1.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1, 2, 1), sd = c(1.5, 1.5, 2.5, 1.5)),
      this.mix.type = 4)$tex.p()
    Output
      \begin{align}
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = 1, & \sigma_{1,2} = 1.5,\\
      \mu_{2,1} = 2, & \sigma_{2,1} = 2.5,\\
      \mu_{2,2} = 1, & \sigma_{2,2} = 1.5.
      \end{array}
      \end{align}

---

    Code
      a$set.cmp(data.frame(mean = c(1, 1, 1, 2), sd = c(1.5, 1.5, 1.5, 2.5)),
      this.mix.type = 4)$tex.p()
    Output
      \begin{align}
      \Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2,\\
      \\
      \Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x),\\
      \Phi_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_{i,j}^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu_{i,j})^2}{2 \sigma_{i,j}^2} \right) dt,\\
      \Phi^\ast_{i,j}(x) &= \dfrac{1}{\sqrt{2 \pi \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2}} \int_{-\infty}^{x} \exp \left( \begin{array}{c} -\dfrac{(t - \mu_{i,j})^2}{2 \left( \begin{array}{c} \dfrac{\sigma_{i,j}}{\sqrt{2}} \end{array} \right)^2} \end{array} \right) dt,\\
      \\
      & \begin{array}{ll}
      \mu_{1,1} = 1, & \sigma_{1,1} = 1.5,\\
      \mu_{1,2} = 1, & \sigma_{1,2} = 1.5,\\
      \mu_{2,1} = 1, & \sigma_{2,1} = 1.5,\\
      \mu_{2,2} = 2, & \sigma_{2,2} = 2.5.
      \end{array}
      \end{align}

