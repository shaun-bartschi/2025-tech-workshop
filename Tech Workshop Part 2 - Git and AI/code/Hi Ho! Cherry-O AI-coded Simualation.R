# spinner() function:
# This function simulates one spin of the Hi Ho! Cherry-O spinner.
# It takes the current number of cherries on the tree and in the bucket
# as input, applies the chosen action, and returns the updated counts.
#
# Arguments:
#   cherries_on_tree: The current number of cherries remaining on the tree (integer).
#   cherries_in_bucket: The current number of cherries in the player's bucket (integer).
#
# Returns:
#   A list containing two elements:
#     - 'cherries_on_tree': The updated number of cherries on the tree.
#     - 'cherries_in_bucket': The updated number of cherries in the bucket.
spinner <- function(cherries_on_tree, cherries_in_bucket) {
     # Define the possible spinner outcomes.
     # Each outcome has an equal probability since there are 7 options.
     outcomes <- c(
          "take_4", "take_3", "take_2", "take_1",
          "bird", "dog", "basket"
     )
     
     # Randomly select one outcome.
     spin_result <- sample(outcomes, 1)
     
     # Apply the effect of the spin_result
     if (spin_result == "take_4") {
          # Take 4 cherries off the tree.
          # Ensure cherries_on_tree does not go below 0.
          cherries_taken <- min(cherries_on_tree, 4)
          cherries_on_tree <- cherries_on_tree - cherries_taken
          cherries_in_bucket <- cherries_in_bucket + cherries_taken
     } else if (spin_result == "take_3") {
          # Take 3 cherries off the tree.
          cherries_taken <- min(cherries_on_tree, 3)
          cherries_on_tree <- cherries_on_tree - cherries_taken
          cherries_in_bucket <- cherries_in_bucket + cherries_taken
     } else if (spin_result == "take_2") {
          # Take 2 cherries off the tree.
          cherries_taken <- min(cherries_on_tree, 2)
          cherries_on_tree <- cherries_on_tree - cherries_taken
          cherries_in_bucket <- cherries_in_bucket + cherries_taken
     } else if (spin_result == "take_1") {
          # Take 1 cherry off the tree.
          cherries_taken <- min(cherries_on_tree, 1)
          cherries_on_tree <- cherries_on_tree - cherries_taken
          cherries_in_bucket <- cherries_in_bucket + cherries_taken
     } else if (spin_result == "bird" || spin_result == "dog") {
          # Bird or Dog: Put 2 cherries back ON the tree from the bucket.
          # Ensure cherries_in_bucket does not go below 0.
          # Ensure cherries_on_tree does not exceed 10 (the starting amount).
          cherries_returned <- min(cherries_in_bucket, 2)
          cherries_in_bucket <- cherries_in_bucket - cherries_returned
          cherries_on_tree <- min(10, cherries_on_tree + cherries_returned)
     } else if (spin_result == "basket") {
          # Basket: Put ALL cherries in your bucket back onto the tree.
          # Ensure cherries_on_tree does not exceed 10.
          cherries_on_tree <- min(10, cherries_on_tree + cherries_in_bucket)
          cherries_in_bucket <- 0 # Bucket is now empty
     }
     
     # Return the updated counts of cherries.
     return(list(
          cherries_on_tree = cherries_on_tree,
          cherries_in_bucket = cherries_in_bucket
     ))
}

# play_game() function:
# This function simulates a full game of Hi Ho! Cherry-O until all cherries
# are removed from the tree.
#
# Returns:
#   The number of spins required to win the game (integer).
play_game <- function() {
     # Initialize the game state.
     cherries_on_tree <- 10
     cherries_in_bucket <- 0
     spins <- 0
     
     # Continue spinning until all cherries are off the tree.
     while (cherries_on_tree > 0) {
          spins <- spins + 1 # Increment spin count for each turn
          
          # Call the spinner function to get the result of the current spin.
          # The spinner returns a list, so we unpack it.
          updated_state <- spinner(cherries_on_tree, cherries_in_bucket)
          cherries_on_tree <- updated_state$cherries_on_tree
          cherries_in_bucket <- updated_state$cherries_in_bucket
          
          # Optional: Print current state for debugging/visualization
          # cat("Spin:", spins, "| Cherries on Tree:", cherries_on_tree,
          #     "| Cherries in Bucket:", cherries_in_bucket, "\n")
     }
     
     # Return the total number of spins taken to win.
     return(spins)
}

# Example of how to play one game and see the result:
num_spins <- play_game()
print(paste("It took", num_spins, "spins to win the game!"))

# Example of how to run multiple simulations (e.g., 1000 times)
# and get statistics like the average number of spins:
num_simulations <- 100000
results <- replicate(num_simulations, play_game())
print(paste("Average spins over", num_simulations, "games:", mean(results)))
print(paste("Median spins over", num_simulations, "games:", median(results)))
print(paste("Max spins over", num_simulations, "games:", max(results)))


# Now we will plot a histogram of the results to visualize the distribution of spins.
library(ggplot2)
results_df <- data.frame(spins = results)
ggplot(results_df, aes(x = spins)) +
     geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
     labs(title = "Distribution of Spins in Hi Ho! Cherry-O",
          x = "Number of Spins",
          y = "Frequency") +
     theme_minimal()
# Note: Make sure to have ggplot2 installed to run the plotting code.
# If ggplot2 is not installed, you can install it using:

#Finally, we will fit a chi squared distribution to the histogram, as well as a density curve in different colors
# install.packages("ggplot2")
library(MASS) # For fitdistr function
fit <- fitdistr(results, "chisq", start = list(df = 2))
ggplot(results_df, aes(x = spins)) +
     geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
     stat_function(fun = dchisq, args = list(df = fit$estimate["df"]), color = "red", size = 1) +
     labs(title = "Distribution of Spins in Hi Ho! Cherry-O with Chi-Squared Fit",
          x = "Number of Spins",
          y = "Density") +
     theme_minimal()
