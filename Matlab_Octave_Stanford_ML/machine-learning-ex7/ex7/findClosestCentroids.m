function idx = findClosestCentroids(X, centroids)
%FINDCLOSESTCENTROIDS computes the centroid memberships for every example
%   idx = FINDCLOSESTCENTROIDS (X, centroids) returns the closest centroids
%   in idx for a dataset X where each row is a single example. idx = m x 1 
%   vector of centroid assignments (i.e. each entry in range [1..K])
%

% Set K
K = size(centroids, 1);

% You need to return the following variables correctly.
idx = zeros(size(X,1), 1);

% ====================== YOUR CODE HERE ======================
% Instructions: Go over every example, find its closest centroid, and store
%               the index inside idx at the appropriate location.
%               Concretely, idx(i) should contain the index of the centroid
%               closest to example i. Hence, it should be a value in the 
%               range 1..K
%
% Note: You can use a for-loop over the examples to compute this.
%
 m = size(X,1);
 for i=1:m
   % Assign to the first Centroid
   k = 1;
   min_dist = sum((X(i,:) - centroids(1,:)) .^ 2);
   %% Loop over Different Centroid Locations to find the Closest Centroid
   for j=2:K
       dist = sum((X(i,:) - centroids(j,:)) .^ 2);
       if(dist < min_dist)
          min_dist = dist;
          k = j;
       end
   end
%% Assign the value of the Centroid to the Index Matrix C(i)   
   idx(i)= k;   
end



% =============================================================

end
