# A Bayesian approach to obtaining uncertainty estimates from neural networks
# https://blogs.rstudio.com/tensorflow/posts/2018-11-12-uncertainty_estimates_dropout/
# modified by Wim Ikbal (2019)
# Bagian I
# Citation: Keydana (2018, Nov. 12). TensorFlow for R: You sure? A Bayesian approach to obtaining uncertainty estimates from neural networks. Retrieved from https://blogs.rstudio.com/tensorflow/posts/2018-11-12-uncertainty_estimates_dropout/

ConcreteDropout <- R6::R6Class("ConcreteDropout",
   inherit = KerasWrapper, public = list(
                           weight_regularizer = NULL,
                           dropout_regularizer = NULL,
                           init_min = NULL,
                           init_max = NULL,
                           is_mc_dropout = NULL,
                           supports_masking = TRUE,
                           p_logit = NULL,
                           p = NULL,
   
   initialize = function(weight_regularizer,
                         dropout_regularizer,
                         init_min,
                         init_max,
                         is_mc_dropout) {
     self$weight_regularizer <- weight_regularizer
     self$dropout_regularizer <- dropout_regularizer
     self$is_mc_dropout <- is_mc_dropout
     self$init_min <- k_log(init_min) - k_log(1 - init_min)
     self$init_max <- k_log(init_max) - k_log(1 - init_max)
   },
   
   build = function(input_shape) {
     super$build(input_shape)
     
     self$p_logit <- super$add_weight(
       name = "p_logit",
       shape = shape(1),
       initializer = initializer_random_uniform(self$init_min, self$init_max),
       trainable = TRUE
     )
     
     self$p <- k_sigmoid(self$p_logit)
     
     input_dim <- input_shape[[2]]
     
     weight <- private$py_wrapper$layer$kernel
     
     kernel_regularizer <- self$weight_regularizer * 
       k_sum(k_square(weight)) / 
       (1 - self$p)
     
     dropout_regularizer <- self$p * k_log(self$p)
     dropout_regularizer <- dropout_regularizer +  
       (1 - self$p) * k_log(1 - self$p)
     dropout_regularizer <- dropout_regularizer * 
       self$dropout_regularizer * 
       k_cast(input_dim, k_floatx())
     
     regularizer <- k_sum(kernel_regularizer + dropout_regularizer)
     super$add_loss(regularizer)
   },
   
   concrete_dropout = function(x) {
     eps <- k_cast_to_floatx(k_epsilon())
     temp <- 0.1
     
     unif_noise <- k_random_uniform(shape = k_shape(x))
     
     drop_prob <- k_log(self$p + eps) - 
       k_log(1 - self$p + eps) + 
       k_log(unif_noise + eps) - 
       k_log(1 - unif_noise + eps)
     drop_prob <- k_sigmoid(drop_prob / temp)
     
     random_tensor <- 1 - drop_prob
     
     retain_prob <- 1 - self$p
     x <- x * random_tensor
     x <- x / retain_prob
     x
   },
   
   call = function(x, mask = NULL, training = NULL) {
     if (self$is_mc_dropout) {
       super$call(self$concrete_dropout(x))
     } else {
       k_in_train_phase(
         function()
           super$call(self$concrete_dropout(x)),
         super$call(x),
         training = training
       )
     }
   }
 )
)

# function for instantiating custom wrapper
layer_concrete_dropout <- function(object, 
                                   layer,
                                   weight_regularizer = 1e-6,
                                   dropout_regularizer = 1e-5,
                                   init_min = 0.1,
                                   init_max = 0.1,
                                   is_mc_dropout = TRUE,
                                   name = NULL,
                                   trainable = TRUE) {
  create_wrapper(ConcreteDropout, object, list(
    layer = layer,
    weight_regularizer = weight_regularizer,
    dropout_regularizer = dropout_regularizer,
    init_min = init_min,
    init_max = init_max,
    is_mc_dropout = is_mc_dropout,
    name = name,
    trainable = trainable
  ))
}

# heterescedastic loss function ----

heteroscedastic_loss <- function(y_true, y_pred) {
  mean <- y_pred[, 1:output_dim]
  log_var <- y_pred[, (output_dim + 1):(output_dim * 2)]
  precision <- k_exp(-log_var)
  k_sum(precision * (y_true - mean) ^ 2 + log_var, axis = 2)
}
