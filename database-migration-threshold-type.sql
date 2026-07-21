-- Migration: Add threshold_type column to thresholds table
-- Date: 2026-07-21
-- Description: Replace is_final boolean with threshold_type enum to support experimental, preliminary, and final thresholds

-- Step 1: Add the new threshold_type column
ALTER TABLE thresholds ADD COLUMN IF NOT EXISTS threshold_type TEXT;

-- Step 2: Create an enum type for threshold types (optional, but recommended for data integrity)
-- Note: In PostgreSQL, you would typically create a proper ENUM type, but Hasura works with TEXT columns
-- For PostgreSQL:
-- CREATE TYPE threshold_type_enum AS ENUM ('experimental', 'preliminary', 'final');
-- ALTER TABLE thresholds ADD COLUMN threshold_type threshold_type_enum;

-- Step 3: Migrate existing data from is_final to threshold_type
-- For existing records where is_final = true, set threshold_type = 'final'
UPDATE thresholds SET threshold_type = 'final' WHERE is_final = true;

-- For existing records where is_final = false, set threshold_type = 'preliminary'
-- This assumes all non-final thresholds were preliminary
UPDATE thresholds SET threshold_type = 'preliminary' WHERE is_final = false AND threshold_type IS NULL;

-- Step 4: Add a default value for new records (optional)
ALTER TABLE thresholds ALTER COLUMN threshold_type SET DEFAULT 'experimental';

-- Step 5: Add a check constraint to ensure only valid values are stored (optional but recommended)
ALTER TABLE thresholds ADD CONSTRAINT check_threshold_type 
CHECK (threshold_type IN ('experimental', 'preliminary', 'final'));

-- Step 6: Create indexes for better query performance (optional but recommended)
CREATE INDEX IF NOT EXISTS idx_thresholds_label_model_type ON thresholds(label_id, model_id, threshold_type);
CREATE INDEX IF NOT EXISTS idx_thresholds_type ON thresholds(threshold_type);

-- Note: The is_final column can be kept for backwards compatibility or dropped if no longer needed
-- To drop the old column (after verifying migration is complete):
-- ALTER TABLE thresholds DROP COLUMN IF EXISTS is_final;
