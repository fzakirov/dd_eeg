library(ggplot2)

path = "/users/dd_eeg/data"

# FEEDBACK-RELATED MEASURES
csv_name <- sprintf("%s/theta_power.csv", path) # filename with dataframe of interest (change to delta_power) for delta

df <- read.csv(csv_name)

# take only IDs with sufficient trial count
valid_id <- c('LV001', 'AM003', 'DK007', 'AM004', 'AM005', 'AR004', 'AR005',
              'GP001', 'DK004', 'DK009', 'AR006', 'AD006', 'FZ002', 'DK011',
              'FZ003', 'DK012', 'DM002', 'DK013', 'DM003', 'AM006', 'AM007',
              'DM006', 'GP002', 'OS011', 'AM009', 'AM008', 'FZ005', 'FZ006',
              'FZ008', 'FZ007', 'FZ009', 'FZ016', 'AR007', 'FZ017', 'FZ018',
              'FZ019', 'FZ020', 'FZ021', 'EP002', 'EP003', 'DM008', 'DK010',
              'DK006', 'OS008', 'FZ013', 'EP005', 'OS006', 'DM005', 'FZ010',
              'FZ015', 'EP004', 'FZ004')

# subset only valid IDs
df <- df[df$id %in% valid_id, ] 

# factorize IDs
df$id <- as.factor(df$id)

upper_th <- mean(df$pow)+3*sd(df$pow)
lower_th <- mean(df$pow)-3*sd(df$pow)
df <- df %>% filter(pow <= upper_th & pow >= lower_th) # remove outliers

ggplot(df, aes(x = df, y = pow) )+
  geom_point() +  # Scatter plot of the data points
  geom_smooth(method = "glm", se = TRUE) +  # Add a linear regression line
  labs(title = "Power",
       x = "DF",
       y = "Power") +
  theme_minimal() + 
  theme(axis.title = element_text(face = "plain"),
        legend.title = element_text(face = "plain"),
        panel.border = element_rect(color = "grey", fill = NA),
        plot.title = element_text(hjust = 0.5, face = "plain"))# Optional: apply a minimal theme


# ANTICIPATORY ALPHA PSD
csv_name <- sprintf("%s/alpha_poz_raw.csv", path)

df <- read.csv(csv_name)

df$psd = df$psd * 1e12

# Convert delay to a factor
df$delay <- as.factor(df$delay)
df$id <- as.factor(df$id)

upper_th <- mean(df$psd)+3*sd(df$psd)
lower_th <- mean(df$psd)-3*sd(df$psd)
df <- df %>% filter(psd <= upper_th & psd >= lower_th)


ggplot(df, aes(x = df, y = psd) )+
  geom_point() +  # Scatter plot of the data points
  geom_smooth(method = "glm", se = TRUE) +  # Add a linear regression line
  labs(title = "Alpha band PSD",
       x = "DF",
       y = "μV²/Hz") +
  theme_minimal() + 
  theme(axis.title = element_text(face = "plain"),
        legend.title = element_text(face = "plain"),
        panel.border = element_rect(color = "grey", fill = NA),
        plot.title = element_text(hjust = 0.5, face = "plain"))# Optional: apply a minimal theme
