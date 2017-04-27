    function [C, sigma] = dataset3Params(X, y, Xval, yval)
%DATASET3PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = DATASET3PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

C_values = [0.01 0.03 0.1 0.3 1 3 10 30];
Sigma_values = [0.01 0.03 0.1 0.3 1 3 10 30];
error_min = inf;


fprintf('Trying Different C and Sigma values\n');
for C = C_values
  for sigma = Sigma_values
    fprintf('Iteration with  C = %f, sigma = %f ',C,sigma);
    model = svmTrain(X, y, C, @(x1, x2) gaussianKernel(x1, x2, sigma));
    err   = mean(double(svmPredict(model, Xval) ~= yval));
    if( err <= error_min )
      C_min     = C;
      sigma_min = sigma;
      error_min   = err;
      fprintf('Min.Err found with C= %f, sigma = %f,  and Error = %f',C_min, sigma_min, error_min);
    end
  end
end
C     = C_min;
sigma = sigma_min;

fprintf('Best Fit value C = %f, sigma = %f  and prediction error = %f\n', C,sigma, error_min);





% =========================================================================

end