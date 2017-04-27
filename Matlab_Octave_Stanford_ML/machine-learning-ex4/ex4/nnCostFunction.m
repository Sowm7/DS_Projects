function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%NNCOSTFUNCTION Implements the neural network cost function for a two layer
%neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices. 
% 
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
m = size(X, 1);
         
% You need to return the following variables correctly 
J = 0;
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m
%

a1 = [ones(m, 1) X];  
z2 = a1 * Theta1' ;  
a2 = sigmoid(z2);  
a2 = [ones(m,1) a2];  
z3 = a2 * Theta2';  
a3 = sigmoid(z3);  
h = a3; 

Y = zeros(size(a3));

for i = 1:num_labels
	Y(:, i) = (y == i); 
end



reg_term = (sum(sum(Theta1(:, 2:end).^2, 2))+sum(sum(Theta2(:, 2:end).^2, 2))) * lambda/(2*m);

J = sum(sum((-Y).*log(h) - (1-Y).*log(1-h), 2))/m + reg_term;



% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a 
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the 
%               first time.
%
% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%


%for i = 1:m

%a1 = X(i,:); 
%a1 = [1 a1];
% 1x401 
%z2 = a1 * Theta1' ;
%%%% 1x 	401 * 401x 25 = 1x25 vector
%a2 = sigmoid(z2);
%a2 = [1 a2]; 
%% add bias unit , a2 is now a 1x26 vector

%z3 = a2 * Theta2' ; 
%%%%1x26 *26x10 = 1x10

%a3 = sigmoid(z3); 
%%% a3 is a 1x10 vector

%d3 = (a3 - Y(i,:))'; % 10x1
 
%d2 = (Theta2'*d3 ).* (sigmoidGradient([1 z2]))'; 
%%% (26x10 * 10x1) .* 26x1 = 26x1 .* 26x1


%Theta1_grad = Theta1_grad + d2(2:end) * a1;
%%% 25x1 * 1x401 = 25x401
%Theta2_grad = Theta2_grad + d3 * a2 ;
%%% 10x1 * 1x26 = 10x26

%end

%Theta1_grad(:,1) = 1/m * Theta1_grad(:,1);
%Theta2_grad(:,1) = 1/m * Theta2_grad(:,1);

%Theta1_grad(:,2:end) = (lambda/m)*(Theta1_grad(:,2:end) );
%Theta2_grad(:,2:end) = (lambda/m)*(Theta2_grad(:,2:end) );
%========= For Loop Method ========================
delta3 = (a3 - Y)';
sgz2 = sigmoidGradient(z2);
delta2 = (Theta2(:, 2:end)' * delta3) .* sgz2';

DELTA1 = delta2 * a1; %25x401
DELTA2 = delta3 * a2; %10x26

reg1 = (lambda / m) * [zeros(size(Theta1, 1),1) Theta1(:,2:end)];
reg2 = (lambda / m) * [zeros(size(Theta2, 1),1) Theta2(:,2:end)];

Theta1_grad = (1 / m) * DELTA1 + reg1;
Theta2_grad = (1 / m) * DELTA2 + reg2;










% -------------------------------------------------------------

% =========================================================================

% Unroll gradients
grad = [Theta1_grad(:) ; Theta2_grad(:)];


end
